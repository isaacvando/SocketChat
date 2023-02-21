import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as B

main :: IO ()
main = do
  putStrLn "My chat room client. Version One."
  sock <- getSock
  send sock "foo"
  close sock

  sock' <- getSock
  -- msg <- B.getLine
  -- send sock' msg
  send sock' "bar"
  close sock'

getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  -- setSocketOption sock ReuseAddr 1
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock

loop :: Socket -> IO ()
loop sock = do
  -- msg <- recv sock 4096
  -- if msg == "" then return () else print msg
  -- send sock 
  input <- B.getLine
  send sock input
  loop sock