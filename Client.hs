import Network.Socket
import Network.Socket.ByteString ( recv, send )
import qualified Data.ByteString.UTF8 as U
import Control.Concurrent.Async (race_)
import Control.Exception (bracket)


main :: IO ()
main = do
  putStrLn "My chat room client. Version Two."
  bracket 
    getSock 
    (\s -> send s (U.fromString "logout") >> close s)
    (\s -> race_ (sendMsg s) (recvMsg s))


sendMsg :: Socket -> IO ()
sendMsg sock = do
  input <- getLine
  if input == "logout"
    then return ()
    else do
      send sock (U.fromString input)
      sendMsg sock


recvMsg :: Socket -> IO ()
recvMsg sock = do
  msg <- U.toString <$> recv sock 4096
  putStrLn msg
  recvMsg sock


getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock
