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
    (\s -> race_ (sendLoop s) (recvLoop s))


sendLoop :: Socket -> IO ()
sendLoop sock = do
  input <- getLine
  case words input of
    ["logout"] -> return ()
    "send":_ -> talk input
    ["newuser", _, _] -> talk input
    ["login", _, _] -> talk input
    ["who"] -> talk input
    _ -> putStrLn ("\"" ++ input ++ "\" is not a valid command.") >> sendLoop sock

  where
    talk s = send sock (U.fromString s) >> sendLoop sock


recvLoop :: Socket -> IO ()
recvLoop sock = do
  msg <- U.toString <$> recv sock 4096
  putStrLn msg
  recvLoop sock


getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock
