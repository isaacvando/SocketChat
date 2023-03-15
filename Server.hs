import Network.Socket
    ( getAddrInfo,
      setSocketOption,
      accept,
      bind,
      listen,
      socket,
      close,
      defaultProtocol,
      AddrInfo(addrAddress),
      SocketOption(ReuseAddr),
      Family(AF_INET),
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (send, recv)
import Data.List.Split (splitOn)
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as U
import System.IO.Strict as S (readFile)
import Control.Monad 
import Control.Concurrent
import Control.Exception (SomeException)
import qualified Data.Map as Map
import Control.Concurrent.STM



data Server = Server
  {
    clients :: TVar (Map.Map String Client)
    , users :: TVar [(String,String)]
  }

data Client = Client
  {
    clientSocket :: Socket
    , clientSendChan :: TChan String
  }



main :: IO ()
main = do
  putStrLn "My chat room server. Version One."
  listener <- getSock
  server <- newServer
  forever $ do
    (conn,_) <- accept listener
    forkFinally (talk server conn) (\_ -> close conn)


newServer :: IO Server
newServer = do
  clientTVar <- newTVarIO Map.empty
  userList <- parseUsers <$> S.readFile "users.txt"
  userTVar <- newTVarIO userList
  return Server { clients = clientTVar, users = userTVar}


talk :: Server -> Socket -> IO ()
talk server conn = do
  msg <- U.toString <$> recv conn 4096
  process server conn msg
  talk server conn
  -- let str = U.toString msg
  -- putStrLn $ show conn ++ ": " ++ str
  -- if str == "logout" || str == "" 
  --   then return () 
  --   else talk server conn

process :: Server -> Socket -> String -> IO ()
process server@Server{..} conn msg = case words msg of
  ["login", user, pass] -> if (user,pass) `elem` users then addClient user conn
  ["send", "all", x] -> broadcast server x
  _ -> return ()


broadcast :: Server -> String -> IO ()
broadcast server msg = 

  



-- process :: State -> String -> (State, String, String)
-- process st msg = case (loggedIn st, (words . drop keyLength) msg) of
--   ("", "send":_) -> 
--     (st, "Denied. Please login first.", "")
--   (name, "send":xs) -> 
--     let reply = name ++ ": " ++ unwords xs 
--     in (st, reply, reply)

--   (_, ["newuser", name, pass]) -> if (name, pass) `elem` users st 
--     then (st, "Denied. User account already exists.", "")
--     else (st {users = (name, pass) : users st}, "New user account created. Please login.", "New user account created.")

--   ("", ["login", name, pass]) -> if (name,pass) `elem` users st
--     then (st {loggedIn = name}, "login confirmed", name ++ " login.")
--     else (st, "Denied. User name or password incorrect.", "")
--   (name, "login":_) -> 
--     (st, "Denied. User " ++ name ++ " is already logged in.", "")

--   ("", ["logout"]) -> 
--     (st, "No user to logout.", "")
--   (name, ["logout"]) -> 
--     (st {loggedIn = ""}, name ++ " left.", name ++ " logout.")

--   (_, xs) -> (st, "\"" ++ unwords xs ++ "\" is not a valid command.", "")


-- renderUsers :: State -> String
-- renderUsers = unlines . map go . users
--   where
--     go (name,pass) = "(" ++ name ++ ", " ++ pass ++ ")"


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


  
  
  -- msg <- recv conn 4096
  -- let (state', resp, echo) = process state (U.toString msg)
  -- _ <- send conn (U.fromString resp)
  -- unless (null echo) $ putStrLn echo
  -- threadDelay 10000000
  -- putStrLn "foobar my doggie"
  -- _ <- send conn (U.fromString "part 2 baybe")
  -- close conn
  -- writeFile "users.txt" $ renderUsers state'
  -- return state'