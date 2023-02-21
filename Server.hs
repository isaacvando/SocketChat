import Network.Socket
import Network.Socket.ByteString
import Data.List.Split (splitOn)
import qualified Control.Exception as E
-- import Data.Maybe (isJust)
import qualified Data.ByteString.UTF8 as U

data State = State { loggedIn :: Maybe String, users :: [(String,String)]}

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  putStrLn "My chat room server. Version One."
  let state = State {loggedIn = Nothing, users = users}
  E.bracket getSock close (runServer server state)
  return ()


runServer :: (State -> Socket -> IO State) -> State -> Socket -> IO State
runServer server state sock = do
  state' <- server state sock
  runServer server state' sock

server :: State -> Socket -> IO State
server state sock = do
  (conn,_) <- accept sock
  msg <- recv conn 4096
  state' <- process (U.toString msg) conn
  close conn
  return state'
  where
    process msg conn = case (loggedIn state, words msg) of 
      (Just name, "send":xs) -> talk conn (name ++ ": " ++ unwords xs) >> return state
      (Nothing, "send":_) -> talk conn ("Denied. Please login first.") >> return state
      (_, xs) -> send conn (U.fromString $ ("\"" ++ unwords xs ++ "\" is not a valid command.")) >> return state

talk :: Socket -> String -> IO Int
talk conn msg = send conn (U.fromString msg)

getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  setSocketOption sock ReuseAddr 1
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
