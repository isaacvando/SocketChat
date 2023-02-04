import Network.Socket
import Network.Socket.ByteString

main :: IO ()
main = do
  putStrLn "My chat room client. Version One.\n"
  sock <- socket AF_INET Stream defaultProtocol
  -- setSocketOption sock ReuseAddr 1
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  send sock "bazbarfoo"
  loop sock
  close sock

loop :: Socket -> IO ()
loop sock = do
  msg <- recv sock 4096
  print msg
  loop sock