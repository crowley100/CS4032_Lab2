import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B8
import System.IO
import System.Posix.Unistd

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                msgSender sock

msgSender :: Socket -> IO ()
msgSender sock = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  --hPutStrLn handle "GET /server.hs?message=its+high+noon HTTP/1.0\r\n\r\n"
  rMsg <- hGetContents handle
  putStrLn rMsg
  hClose handle
  
main :: IO ()
main = client "localhost" 8000
