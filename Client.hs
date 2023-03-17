import Network.Socket
    ( getAddrInfo,
      connect,
      socket,
      close,
      defaultProtocol,
      AddrInfo(addrAddress),
      Family(AF_INET),
      Socket,
      SocketType(Stream) )
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
    ["logout"] 
      -> talk input

    "send":xs 
      | length (unwords xs) > 256 || null (unwords xs)
        -> retry "The message must be between 1 and 256 characters in length."
      | otherwise
        -> go

    ["newuser", user, pass]
      | length user < 3 || length user > 32 
        -> retry "Username must be between 3 and 32 characters long"

      | length pass < 4 || length pass > 8 
        -> retry "Password must be between 4 and 8 characters long"

      | otherwise 
        -> go

    ["login", _, _] 
      -> go

    xs -> retry ("\"" ++ unwords xs ++ "\" is not a valid command.")
  
  where
    retry s = putStrLn s >> runClient


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
