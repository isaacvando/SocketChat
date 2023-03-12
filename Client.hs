import Network.Socket
    -- ( getAddrInfo,
    --   connect,
    --   socket,
    --   close,
    --   defaultProtocol,
    --   AddrInfo(addrAddress),
    --   Family(AF_INET),
    --   Socket,
    --   SocketType(Stream) )
import Network.Socket.ByteString ( recv, send )
import qualified Data.ByteString.UTF8 as U


main :: IO ()
main = do
  putStrLn "My chat room client. Version One."
  runClient


runClient :: IO ()
runClient = do
  input <- getLine
  let go = talk input >> runClient
  case words input of
    ["logout"] -> talk input
    "send":_ -> go
    ["newuser", _, _] -> go
    ["login", _, _] -> go
    xs -> putStrLn ("\"" ++ unwords xs ++ "\" is not a valid command.") >> runClient


talk :: String -> IO ()
talk msg = do
  sock <- getSock
  _ <- send sock (U.fromString msg)
  recv sock 4096 >>= (putStrLn . U.toString)
  close sock


getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock
