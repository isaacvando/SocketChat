import Network.Socket
import Network.Socket.ByteString
import Data.List.Split (splitOn)

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  sock <- getSock
  runServer sock server

runServer :: Socket -> (Socket -> IO ()) -> IO ()
runServer s server = do
  server s
  runServer s server

server :: Socket -> IO ()
server sock = do
  (conn,_) <- accept sock
  recv conn 4096 >>= print
  close conn

getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  -- print $ addrAddress hostAddr
  bind sock (addrAddress hostAddr)
  listen sock 1
  return sock


loop :: Socket -> IO ()
loop sock = do
  (conn, _) <- accept sock
  -- send conn "foobarbaz"
  recv conn 4096 >>= print
  close conn
  loop sock

parseUsers :: String -> [(String, String)]
parseUsers xs = map f (lines xs)
  where 
    f x = (head splits, concat (tail splits))
      where
        trimmed = (init . tail) x 
        splits = splitOn ", " trimmed
