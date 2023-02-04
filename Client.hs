import Network.Socket
import Network.Socket.ByteString

main :: IO ()
main = do
  putStrLn "My chat room client. Version One.\n"
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  send sock "bazbarfoo"
  close sock
