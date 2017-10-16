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

    let rooms = "":rooms --hacky
    hPutStrLn hdl "You are now connected to the server"
    --hPutStrLn hdl "JOIN_CHATROOM: [chatroom name]\nCLIENT_IP: [IP Address of client if UDP | 0 if TCP]\nPORT: [port number of client if UDP | 0 if TCP]\nCLIENT_NAME: [string Handle to identifier client user]"
    name <- fmap init (hGetLine hdl)


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
        let msg = ["this", "is", "a", "test"]
        let a = unlines msg
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"

             "KILL_SERVICE" -> close parentSock

             "HELO text" -> heloText hdl addr >> loop

             _      -> outputParser line chan joinId >> loop
        ))



    killThread reader                      -- kill after the loop ends
    --broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
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
  let Just room = stripPrefix "CHAT: " (msg !! 0)
  if (True)---room `elem` rooms)
  then hPutStrLn hdl (line)
  else return()
  --when (room `elem` rooms) --fails

outputParser :: String -> Chan Msg -> Int -> IO()
outputParser a chan joinId = do
  let broadcast msg = writeChan chan (joinId, msg)
  --let a = unlines line
  broadcast(a)

  return ()



--line <- fmap init (hGetLine hdl)
--    let Just room = stripPrefix "JOIN_CHATROOM: " line
--    let rooms = room:rooms
--    line <- fmap init (hGetLine hdl)
--    let Just ip = stripPrefix "CLIENT_IP: " line
--    line <- fmap init (hGetLine hdl)
--    let Just port = stripPrefix "PORT: " line
--    line <- fmap init (hGetLine hdl)
--    let Just name = stripPrefix "CLIENT_NAME: " line

--    hPutStr hdl ("JOINED_CHATROOM: "++rooms !! 0++"\nCLIENT_IP: "++ip++"\nPORT: "++port++"\nROOM_REF: \nJOIN_ID: \n")

