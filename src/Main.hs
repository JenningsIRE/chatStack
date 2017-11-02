{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams
   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

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

main :: IO ()
main = withSocketsDo $ do
  port <- getArgs
  sock <- listenOn (PortNumber (fromIntegral (read (head port))))
  server <- newServer sock
  printf "Listening on port %s\n" (head port)
  mainLoop server sock (read (head port))  0

mainLoop :: Server -> Socket -> Int -> Int -> IO ()
mainLoop server sock sockPort joinId = do
  (handle, host, port) <- accept sock
  printf "Accepted connection from %s: %s\n" host (show port)
  forkFinally (talk host sockPort handle server joinId) (\_ -> hClose handle)

  mainLoop server sock sockPort $! joinId + 1

type ClientName = String
type RoomName = String
type RoomRef = Int

data Client = Client
  { clientId       :: Int
  , clientIP       :: String
  , clientPort     :: Int
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  , clientRoomRefs :: TVar (Map Int Int)
  }

newClient :: Int -> String -> Int -> Handle -> STM Client
newClient id host port handle = do
  c <- newTChan
  r <- newTVar Map.empty
  return Client { clientId       = id
                , clientIP       = host
                , clientPort     = port
                , clientHandle   = handle
                , clientSendChan = c
                , clientRoomRefs = r
                }

data Server = Server
  { sock      :: Socket
    , clients :: TVar (Map Int Client)
    , rooms   :: TVar (Map RoomName RoomRef)
  }

newServer :: Socket -> IO Server
newServer s  = do
  c <- newTVarIO Map.empty
  r <- newTVarIO Map.empty
  return Server { sock = s, clients = c, rooms = r }

data Message = Broadcast String
             | Command String

broadcast :: Server -> Int -> Message -> STM ()
broadcast Server{..} roomRef msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendToRoom roomRef client msg) (Map.elems clientmap)

sendToRoom :: Int -> Client -> Message -> STM ()
sendToRoom roomRef Client{..} msg = do
  roomRefs <- readTVar clientRoomRefs
  when (roomRef `elem` roomRefs) (writeTChan clientSendChan msg)

talk :: String -> Int -> Handle -> Server -> Int ->  IO ()
talk host port handle server@Server{..} joinId = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  client <- checkAddRoom server joinId host port handle
  runClient server client -- <3>
      `finally` removeClient server joinId

checkAddRoom :: Server -> Int -> String -> Int -> Handle -> IO Client
checkAddRoom server@Server{..} id host port handle = atomically $ do
  clientmap <- readTVar clients
  client <- newClient id host port handle
  writeTVar clients $ Map.insert id client clientmap
  return client

removeClient :: Server -> Int -> IO ()
removeClient server@Server{..} id = atomically $ modifyTVar' clients $ Map.delete id

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
 where
  receive = forever $ do
    msg <- getUserLines clientHandle
    atomically $ writeTChan clientSendChan (Command msg)

  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
        continue <- handleMessage serv client msg
        when continue server

getUserLines :: Handle -> IO String
getUserLines hdl = go hdl ""

go :: Handle -> String -> IO String
go hdl contents = do
  line <- hGetLine hdl
  case line of
               "KILL_SERVICE" -> return "KILL_SERVICE"

               "HELO text" -> return "HELO text"

               "" -> return contents

               _      -> go hdl (contents ++ line ++ "\n")


handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     Broadcast msg -> do
      hPutStrLn clientHandle msg
      return True
     Command msg ->
       case words msg of


           "JOIN_CHATROOM:" : a -> joinChatroom server client msg

           "LEAVE_CHATROOM:" : a -> leaveChatroom server client msg

           "CHAT:" : a -> chat server client msg

           "HELO" : "text" : a -> heloText server client

           "KILL_SERVICE" : a -> killService server client

           _ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True

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
      let r = read room
      atomically $ writeTVar clientRoomRefs $ Map.insert r r roomRefs
      let msg = "JOINED_CHATROOM: "++room ++"\nCLIENT_IP: "++ip++"\nPORT: "++port++"\nROOM_REF: \nJOIN_ID: "++ show clientId ++"\n"
      atomically $ broadcast server r $(Broadcast  msg)


    else hPutStrLn clientHandle $ "Unrecognised command: " ++ a

    return True


leaveChatroom :: Server -> Client -> String -> IO Bool
leaveChatroom server Client{..} a = do
  let l = lines a
  if length l >= 3
  then do
    let room = fromMaybe "" (stripPrefix "LEAVE_CHATROOM: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))

    roomRefs <- atomically $ readTVar clientRoomRefs
    let r = read room
    atomically $ writeTVar clientRoomRefs $ Map.delete r roomRefs
    let msg = "LEFT_CHATROOM: "++room ++"\nJOIN_ID: " ++ show clientId
    atomically $ broadcast server r $(Broadcast  msg)

  else hPutStrLn clientHandle $ "Unrecognised command: " ++ a

  return True

chat :: Server -> Client -> String -> IO Bool
chat server Client{..} a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "CHAT: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))
    let message = fromMaybe "" (stripPrefix "MESSAGE: " (unlines(drop 3 l)))

    let r = read room
    let msg = "CHAT: "++room ++"\nCLIENT_NAME: "++name++"\nMESSAGE: "++message
    atomically $ broadcast server r $(Broadcast  msg)

  else hPutStrLn clientHandle $ "Unrecognised command: " ++ a

  return True

heloText :: Server -> Client  -> IO Bool
heloText server@Server{..} Client{..}  = do
  hPutStrLn clientHandle $ "HELO text\nIP:"++clientIP++"\nPort:"++ show clientPort++"\nStudentID:13326255\n"
  return True

killService :: Server -> Client  -> IO Bool
killService server@Server{..} Client{..}  = do
  sClose sock
  return True