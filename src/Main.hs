{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams
   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

--import ConcurrentUtils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import System.Environment
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import Data.Maybe
import Data.List

{-
Notes
- protocol:
    Server: "Name?"
    Client: <string>
    -- if <string> is already in use, ask for another name
    -- Commands:
    --   /tell <string> message...  (single-user tell)
    --   /quit                      (exit)
    --   /kick <string>             (kick another user)
    --   message...                 (broadcast to all connected users)
- a client needs to both listen for commands from the socket and
  listen for activity from other clients.  Therefore we're going to
  need at least two threads per client (for listening to multiple
  things).  Easiest is to use STM for in-process communication, and to
  have a receiving thread that listens on the socket and forwards to a
  TChan.
- Handle all errors properly, be async-exception safe
- Consistency:
  - if two clients simultaneously kick a third client, only one will be
    successful
See doc/lab-exercises.tex for some ideas for enhancements that you
could try.
-}

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  port <- getArgs
  sock <- listenOn (PortNumber (fromIntegral (read (head port))))
  printf "Listening on port %s\n" (head port)
  mainLoop server sock  0

mainLoop :: Server -> Socket -> Int -> IO ()
mainLoop server sock joinId = do
  (handle, host, port) <- accept sock
  printf "Accepted connection from %s: %s\n" host (show port)
  forkFinally (talk host port handle server joinId) (\_ -> hClose handle)

  mainLoop server sock $! joinId + 1
-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String
type RoomName = String
type RoomRef = Int

data Client = Client
  { clientId       :: Int
  , clientIP       :: String
  , clientPort     :: PortNumber
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  , clientRoomRefs :: TVar ([Int])
  }
-- >>

-- <<newClient
newClient :: Int -> String -> PortNumber -> Handle -> STM Client
newClient id host port handle = do
  c <- newTChan
  r <- newTVar []
  return Client { clientId       = id
                , clientIP       = host
                , clientPort     = port
                , clientHandle   = handle
                , clientSendChan = c
                , clientRoomRefs = r
                }
-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map Int Client)
    , rooms   :: TVar (Map RoomName RoomRef)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  r <- newTVarIO Map.empty
  return Server { clients = c, rooms = r }
-- >>

-- <<Message
data Message = Notice String
             | Broadcast String
             | Command String
-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> Int -> Message -> STM ()
broadcast Server{..} roomRef msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendToRoom roomRef client msg) (Map.elems clientmap)

-- >>


-- <<sendMessage
sendToRoom :: Int -> Client -> Message -> STM ()
sendToRoom roomRef client msg = do
  roomRefs <- readTVar (clientRoomRefs client)
  when (roomRef `elem` roomRefs) (sendMessage client msg)
-- >>

-- <<sendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
-- >>

-- <<sendToName
sendToName :: Server -> Int -> Message -> STM Bool
sendToName server@Server{..} id msg = do
  clientmap <- readTVar clients
  case Map.lookup id clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> return True
-- >>

{-
heloText :: Server -> Client -> Int  -> IO ()
heloText server@Server{..} Client{..} id  = do
  ok <- atomically $ sendToName server id (Tell clientId ("HELO text\nIP:"++clientIP++"\nPort:"++"\nStudentID:13326255\n"))
  if ok
     then return ()
     else hPutStrLn clientHandle (show id ++ " is not connected.")
-}
-- -----------------------------------------------------------------------------
-- The main server

talk :: String -> PortNumber -> Handle -> Server -> Int ->  IO ()
talk host port handle server@Server{..} joinId = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  client <- checkAddRoom server joinId host port handle
  runClient server client -- <3>
      `finally` removeClient server joinId
-- >>

-- <<checkAddClient
checkAddRoom :: Server -> Int -> String -> PortNumber -> Handle -> IO Client
checkAddRoom server@Server{..} id host port handle = atomically $ do
  clientmap <- readTVar clients
  client <- newClient id host port handle
  writeTVar clients $ Map.insert id client clientmap
  broadcast server 0 $ Notice (show id ++ " has connected")
  return client
-- >>

-- <<removeClient
removeClient :: Server -> Int -> IO ()
removeClient server@Server{..} id = atomically $ do
  modifyTVar' clients $ Map.delete id
  broadcast server 0 $ Notice (show id ++ " has disconnected")
-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- getUserLines clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
        continue <- handleMessage serv client msg
        when continue server
-- >>

getUserLines :: Handle -> IO String
getUserLines hdl = go hdl ""


go :: Handle -> String -> IO String
go hdl contents = do
  line <- hGetLine hdl
  case line of
               "quit" -> return "quit"

               "KILL_SERVICE" -> return "KILL_SERVICE"

               "HELO text" -> return "HELO text"

               "" -> return contents

               _      -> go hdl (contents ++ line ++ "\n")


-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Broadcast msg -> output $ msg
     Command msg ->
       case words msg of
           --["KILL_SERVICE"] -> do
             --  atomically $ killService server client
               --return False
           "JOIN_CHATROOM:" : a -> do
              joinChatroom server client msg

            {-
           "LEAVE_CHATROOM:" : a -> do
              atomically $ broadcast server 0 $(leaveChatroom (unwords a))
              return True
           "CHAT:" : a -> do
              atomically $ broadcast server 0 $(chat (unwords a))
              return True

           ["HELO"] -> do
                          heloText server client clientId
                          return False

             -}
           ["quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True

           _ ->return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>


joinChatroom :: Server -> Client -> String -> IO Bool
joinChatroom server Client{..} a = do
    let l = lines a
    if length l >= 4
    then do
      let room = fromMaybe "" (stripPrefix "JOIN_CHATROOM: " (head l))
      let ip = fromMaybe "" (stripPrefix "CLIENT_IP: " (l !! 1))
      let port = fromMaybe "" (stripPrefix "PORT: " (l !! 2))
      let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 3))

      roomRefs <- atomically $ readTVar clientRoomRefs
      atomically $ writeTVar clientRoomRefs $ read room : roomRefs
      let msg = "JOINED_CHATROOM: "++room ++"\nCLIENT_IP: "++ip++"\nPORT: "++port++"\nROOM_REF: \nJOIN_ID: "++ show clientId ++"\n"
      atomically $ broadcast server (read room) $(Broadcast  msg)


    else do
      let msg = "Error"
      atomically $ writeTChan clientSendChan $ Notice msg

    return True

{-
leaveChatroom :: String -> Message
leaveChatroom a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "LEAVE_CHATROOM: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))

    return Broadcast 0 "test" --room ("LEFT_CHATROOM: "++room ++"\nJOIN_ID: " ++ name))

  else return Notice "ERROR"

chat :: String -> Message
chat a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "CHAT: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))
    let message = fromMaybe "" (stripPrefix "MESSAGE: " (unlines(drop 3 l)))

    return Broadcast 0 "test"--room ("CHAT: "++room ++"\nCLIENT_NAME: "++name++"\nMESSAGE: "++message))

  else return Notice "ERROR"
-}
