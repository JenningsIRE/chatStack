module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.List
import Data.Maybe

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  number <- getArgs
  let portNumber = read (head number) :: PortNumber
  bind sock (SockAddrInet portNumber iNADDR_ANY)
  putStrLn ("server open on port " ++ (head number))
  listen sock 2
  chan <- newChan

  --address memory leak
  _ <- forkIO (fix ( \loop -> do
    (_, _) <- readChan chan
    loop))

  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan joinId = do
  conn <- accept sock
  forkIO (runConn conn chan joinId sock)

  mainLoop sock chan $! joinId + 1


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Socket -> IO ()
runConn (sock, addr) chan joinId parentSock = do

    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    let rooms = "general":rooms --hacky
    hPutStrLn hdl "You are now connected to the server"
    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO ( fix ( \loop -> do
        (msgId, line) <- readChan commLine
        if joinId /= msgId
        then  forkIO(inputParser hdl line rooms) >> loop
        else loop
        ))

    handle (\(SomeException _) -> return ()) ( fix ( \loop -> do
        line <- getUserLines hdl
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"

             "KILL_SERVICE" -> close parentSock

             "HELO text" -> heloText hdl addr >> loop

             _      -> outputParser line chan joinId >> loop
        ))

    killThread reader                      -- kill after the loop ends
    hClose hdl                             -- close the handle

getUserLines :: Handle -> IO String
getUserLines hdl = go hdl ""

go :: Handle -> String -> IO String
go hdl contents = do
  line <- fmap init(hGetLine hdl)
  case line of
               "quit" -> return "quit"

               "KILL_SERVICE" -> return "KILL_SERVICE"

               "HELO text" -> return "HELO text"

               "" -> return contents

               _      -> go hdl (contents ++ line ++ "\n")


heloText :: Handle -> SockAddr -> IO()
heloText hdl addr = do
  (Just hostName, Just serviceName) <- getNameInfo [] True True addr
  hPutStr hdl ("HELO text\nIP:"++hostName++"\nPort:"++serviceName++"\nStudentID:13326255\n")

inputParser :: Handle -> String -> [String]-> IO()
inputParser hdl line rooms= do
  let msg = lines line
  let prefixes = words line
  let room = case head prefixes of

              "JOINED_CHATROOM:" -> fromMaybe "" (stripPrefix "JOINED_CHATROOM: " (head msg))

              "LEFT_CHATROOM:" -> fromMaybe "" (stripPrefix "JOINED_CHATROOM: " (head msg))

              "CHAT:" -> fromMaybe "" (stripPrefix "JOINED_CHATROOM: " (head msg))

              _ -> ""

  when (room `elem` rooms) (hPutStrLn hdl line)
  return ()

outputParser :: String -> Chan Msg -> Int -> IO()
outputParser a chan joinId = do
  let broadcast msg = writeChan chan (joinId, msg)
  let x = words a
  if null x
    then return ()
    else case head x of
       "JOIN_CHATROOM:" -> broadcast(concat (joinChatroom a))
       "LEAVE_CHATROOM:" -> broadcast(concat (leaveChatroom a))
       "CHAT:" -> broadcast(concat (chat a))
       --"DISCONNECT:" -> disconnect a)
       _ -> return ()

  return ()

joinChatroom :: String -> [String]
joinChatroom a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "JOIN_CHATROOM: " (head l))
    let ip = fromMaybe "" (stripPrefix "CLIENT_IP: " (l !! 1))
    let port = fromMaybe "" (stripPrefix "PORT: " (l !! 2))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 3))

    return ("JOINED_CHATROOM: "++room ++"\nCLIENT_IP: "++ip++"\nPORT: "++port++"\nROOM_REF: \nJOIN_ID: \n")

  else return "ERROR"

leaveChatroom :: String -> [String]
leaveChatroom a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "LEAVE_CHATROOM: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))

    return ("LEFT_CHATROOM: "++room ++"\nJOIN_ID: " ++ name)

  else return "ERROR"

chat :: String -> [String]
chat a = do
  let l = lines a
  if length l >= 4
  then do
    let room = fromMaybe "" (stripPrefix "CHAT: " (head l))
    let id = fromMaybe "" (stripPrefix "JOIN_ID: " (l !! 1))
    let name = fromMaybe "" (stripPrefix "CLIENT_NAME: " (l !! 2))
    let message = fromMaybe "" (stripPrefix "MESSAGE: " (unlines(drop 3 l)))

    return ("CHAT: "++room ++"\nCLIENT_NAME: "++name++"\nMESSAGE: "++message)

  else return "ERROR"

