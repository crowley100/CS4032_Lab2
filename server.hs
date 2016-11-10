module Main where
 
import System.IO
import Network.Socket
import Control.Concurrent
import Control.ThreadPool

-- init
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0            -- new socket
    setSocketOption sock ReuseAddr 1           -- make socket reuseable
    bind sock (SockAddrInet 8000 iNADDR_ANY)   -- listen on port 8000.
    listen sock 3                             -- up to 10 queued connections
    (input,output) <- threadPoolIO 5 hdlConn
    mainLoop sock input
  
-- handle connections  
mainLoop :: Socket -> Chan (Socket, SockAddr) -> IO ()
mainLoop sock input = do
    conn <- accept sock     -- accept a new client connection
    writeChan input conn
    mainLoop sock input          -- loop

-- server logic
hdlConn :: (Socket, SockAddr) -> IO ()
hdlConn (sock, _) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering -- can change to LineBuffering
    hPutStrLn handle "Hello, world!"
    threadDelay 10000000
    hPutStrLn handle "are you still there?"
    hClose handle
