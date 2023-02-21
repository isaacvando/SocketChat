import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

main :: IO ()
main = do
  putStrLn "My chat room client. Version One."
  runClient
  return ()
  -- sock <- getSock
  -- send sock "foo"
  -- close sock

  -- sock' <- getSock
  -- -- msg <- B.getLine
  -- -- send sock' msg
  -- send sock' "bar"
  -- close sock'

runClient :: IO ()
runClient = do
  input <- getLine
  let go = talk input >> runClient
  case words input of
    ["logout"] -> talk input
    "send":_ -> go
    ["newuser", username, password] -> go
    ["login", username, password] -> go
    xs -> putStrLn ("\"" ++ unwords xs ++ "\" is not a valid command.") >> runClient

talk :: String -> IO ()
talk msg = do
  sock <- getSock
  send sock (U.fromString msg)
  recv sock 4096 >>= (putStrLn . U.toString)

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