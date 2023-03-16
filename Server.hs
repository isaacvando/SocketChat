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
import Control.Concurrent.Async



data Server = Server
  {
    clientsTVar :: TVar (Map.Map String Client)
    , usersTVar :: TVar [(String,String)]
  }

data Client = Client
  {
    clientSocket :: Socket
    , clientSendChan :: TChan String
  }



main :: IO ()
main = do
  putStrLn "My chat room server. Version Two."
  listener <- getSock
  server <- newServer
  forever $ do
    (conn,_) <- accept listener
    forkFinally (runClient server conn) (\_ -> close conn)


newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  users <- parseUsers <$> S.readFile "users.txt"
  u <- newTVarIO users
  return Server { clientsTVar = c, usersTVar = u}


newClient :: Socket -> IO Client
newClient conn = do
  tchan <- newTChanIO
  return Client {clientSocket = conn, clientSendChan = tchan}


runClient :: Server -> Socket -> IO ()
runClient server@Server{..} conn = do
  client <- newClient conn
  user <- login server conn
  clients <- readTVarIO clientsTVar
  atomically $ writeTVar clientsTVar (Map.insert user client clients)
  putStrLn $ user ++ " login."
  forever $ do
    race_ (listenToClient server client) (listenToChannel client)
  where
    listenToChannel Client{..} = forever $ do
      msg <- atomically $ readTChan clientSendChan
      sendStr conn msg


listenToClient :: Server -> Client -> IO ()
listenToClient server@Server{..} client@Client{..} = forever $ do
  msg <- recvStr clientSocket
  atomically $ case words msg of
    ["send","all", x] -> broadcast server x
    ["send", name, x] -> do
      clients <- readTVar clientsTVar
      case Map.lookup name clients of
        Nothing -> writeToChannel ("No user " ++ name ++ " is logged in.") client
        Just c -> writeToChannel x c
    _ -> writeToChannel msg client
    

writeToChannel :: String -> Client -> STM ()
writeToChannel msg Client{..} = writeTChan clientSendChan msg

broadcast :: Server -> String -> STM ()
broadcast Server{..} msg = do
  clients <- readTVar clientsTVar
  mapM_ (writeToChannel msg) (Map.elems clients)


login :: Server -> Socket -> IO String
login server@Server{..} conn = do
  msg <- recvStr conn
  users <- readTVarIO usersTVar
  case words msg of
    ["login", user, pass] -> 
      if (user, pass) `elem` users
        then return user
        else sendStr conn "Denied. User name or password incorrect." >> login server conn
    _ -> sendStr conn "Denied. Please login first." >> login server conn


recvStr :: Socket -> IO String
recvStr conn = U.toString <$> recv conn 4096

sendStr :: Socket -> String -> IO ()
sendStr conn msg = void $ send conn (U.fromString msg)



    -- sendToClient c (U.toString msg)
  -- send conn (U.fromString msg)
  -- broadcast

-- sendToClient :: Client -> String -> IO ()
-- sendToClient client msg = do







  -- case words msg of
  --   ["login", name, pass] -> 
  --   xs -> send conn (U.fromString $ "\"" ++ unwords xs ++ "\" is not a valid command.") >> runClient server conn

  


  -- let str = U.toString msg
  -- putStrLn $ show conn ++ ": " ++ str
  -- if str == "logout" || str == "" 
  --   then return () 
  --   else runClient server conn

-- process :: Server -> Socket -> String -> IO ()
-- process server@Server{..} conn msg = case words msg of
--   ["login", user, pass] -> if (user,pass) `elem` users then addClient user conn
--   ["send", "all", x] -> broadcast server x
--   _ -> return ()


-- broadcast :: Server -> String -> IO ()
-- broadcast server msg = 

  



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