import Network.Socket
import Network.Socket.ByteString
import Data.List.Split (splitOn)
import qualified Control.Exception as E

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  putStrLn "My chat room server. Version One."
  E.bracket getSock close (runServer server)

runServer :: (Socket -> IO ()) -> Socket -> IO ()
runServer server sock = do
  server sock
  runServer server sock

server :: Socket -> IO ()
server sock = do
  (conn,_) <- accept sock
  recv conn 4096 >>= print
  close conn

getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  setSocketOption sock ReuseAddr 1
  -- print $ addrAddress hostAddr
  bind sock (addrAddress hostAddr)
  listen sock 1
  return sock

parseUsers :: String -> [(String, String)]
parseUsers xs = map f (lines xs)
  where 
    f x = (head splits, concat (tail splits))
      where
        trimmed = (init . tail) x 
        splits = splitOn ", " trimmed
