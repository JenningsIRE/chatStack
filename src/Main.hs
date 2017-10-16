module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.List

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  number <- getArgs
  let portNumber = read (number !! 0) :: PortNumber
  bind sock (SockAddrInet portNumber iNADDR_ANY)
  putStrLn ("server open on port " ++ (number !! 0))
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop

  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum sock)

  mainLoop sock chan $! msgNum + 1


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Socket -> IO ()
runConn (sock, addr) chan msgNum parentSock = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Welcome. Please send the following to join a chat room:"
    hPutStrLn hdl "JOIN_CHATROOM: [chatroom name]\nCLIENT_IP: [IP Address of client if UDP | 0 if TCP]\nPORT: [port number of client if UDP | 0 if TCP]\nCLIENT_NAME: [string Handle to identifier client user]"
    line <- fmap init (hGetLine hdl)
    let Just room = stripPrefix "JOIN_CHATROOM: " line
    line <- fmap init (hGetLine hdl)
    let Just ip = stripPrefix "CLIENT_IP: " line
    line <- fmap init (hGetLine hdl)
    let Just port = stripPrefix "PORT: " line
    line <- fmap init (hGetLine hdl)
    let Just name = stripPrefix "CLIENT_NAME: " line

    hPutStr hdl ("JOINED_CHATROOM: "++name++"\nCLIENT_IP: "++ip++"\nPORT: "++port++"\nROOM_REF: \nJOIN_ID: \n")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"

             "KILL_SERVICE" -> close parentSock

             "HELO text" -> heloText hdl addr >> loop

             -- else, continue looping.
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle

heloText :: Handle -> SockAddr -> IO()
heloText hdl addr = do
  (Just hostName, Just serviceName) <- getNameInfo [] True True addr
  hPutStr hdl ("HELO text\nIP:"++hostName++"\nPort:"++serviceName++"\nStudentID:13326255\n")

