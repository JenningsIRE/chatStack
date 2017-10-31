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
import Control.Exception
import Network
import Control.Monad
import Text.Printf

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
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  mainLoop server sock  0

port :: Int
port = 44444
-- >>




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
  , clientRoomRefs :: [Int]
  }
-- >>

-- <<newClient
newClient :: Int -> String -> PortNumber -> Handle -> STM Client
newClient id host port handle = do
  c <- newTChan
  let r = []
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
             | Tell Int String
             | Broadcast Int String
             | Command String
-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
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

tell :: Server -> Client -> Int -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientId msg)
  if ok
     then return ()
     else hPutStrLn clientHandle ((show who) ++ " is not connected.")

heloText :: Server -> Client -> Int  -> IO ()
heloText server@Server{..} Client{..} id  = do
  ok <- atomically $ sendToName server id (Tell clientId ("HELO text\nIP:"++clientIP++"\nPort:"++"\nStudentID:13326255\n"))
  if ok
     then return ()
     else hPutStrLn clientHandle ((show id) ++ " is not connected.")

-- -----------------------------------------------------------------------------
-- The main server

talk :: String -> PortNumber -> Handle -> Server -> Int ->  IO ()
talk host port handle server@Server{..} joinId = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
-- <<readName
  readName = do
    name <- hGetLine handle
    if null name
      then readName
      else mask $ \restore -> do        -- <1>
             ok <- checkAddRoom server joinId host port handle
             case ok of
               Nothing -> restore $ do  -- <2>
                  hPrintf handle
                     "The name %s is in use, please choose another\n" (show joinId)
                  readName
               Just client ->
                  restore (runClient server client) -- <3>
                      `finally` removeClient server joinId
-- >>

-- <<checkAddClient
checkAddRoom :: Server -> Int -> String -> PortNumber -> Handle -> IO (Maybe Client)
checkAddRoom server@Server{..} id host port handle = atomically $ do
  clientmap <- readTVar clients
  client <- newClient id host port handle
  writeTVar clients $ Map.insert id client clientmap
  broadcast server  $ Notice ((show id) ++ " has connected")
  return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> Int -> IO ()
removeClient server@Server{..} id = atomically $ do
  modifyTVar' clients $ Map.delete id
  broadcast server $ Notice ((show id) ++ " has disconnected")
-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
        continue <- handleMessage serv client msg
        when continue $ server
-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell id msg      -> output $ "*" ++ (show id) ++ "*: " ++ msg
     Broadcast id msg -> output $ "<" ++ (show id) ++ ">: " ++ msg
     Command msg ->
       case words msg of
           --["KILL_SERVICE"] -> do
             --  atomically $ killService server client
               --return False
           ["HELO"] -> do
                          heloText server client clientId
                          return False
           --"/tell" : who : what -> do
            --   tell server client who (unwords what)
             --  return True
           ["quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ Broadcast clientId msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>