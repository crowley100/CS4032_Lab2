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
    listen sock 2                              -- 2 queued connections
    mainLoop sock
  
-- handle connections  
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a new client connection
    forkIO (runConn conn)   -- run our server's logic for each client connection in separate thread
    mainLoop sock           -- loop

-- server logic
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering -- can change to LineBuffering
    hPutStrLn handle "Hello, world!"
    hClose handle
