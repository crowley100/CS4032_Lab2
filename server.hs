module Main where
 
import System.IO
import Network.Socket
import Control.Concurrent

-- init
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- new socket
    setSocketOption sock ReuseAddr 1   -- make socket reuseable
    bind sock (SockAddrInet 8000 iNADDR_ANY)   -- listen on port 8000.
    listen sock 3                              -- 2 queued connections
    mainLoop sock
  
-- handle connections  
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a new client connection
    forkIO (hdlConn conn)   -- run our server's logic for each client connection in separate thread
    runConn conn
    mainLoop sock           -- loop

-- server logic
hdlConn :: (Socket, SockAddr) -> IO ()
hdlConn (sock, _) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering -- can change to LineBuffering
    hPutStrLn handle "Hello, world!"
    threadDelay 10000000
    hPutStrLn handle "still there?"
    hClose handle
