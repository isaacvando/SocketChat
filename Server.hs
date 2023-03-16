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
import Data.List
import Data.List.Split (splitOn)
import qualified Data.ByteString.UTF8 as U
import System.IO.Strict as S (readFile)
import Control.Monad 
import Control.Concurrent
import qualified Data.Map.Strict as Map
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
    , clientName :: String
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


newClient :: Socket -> String -> IO Client
newClient conn name = do
  tchan <- newTChanIO
  return Client {clientSocket = conn, clientSendChan = tchan, clientName = name}


runClient :: Server -> Socket -> IO ()
runClient server@Server{..} conn = do
  user <- login server conn
  client <- newClient conn user
  atomically $ modifyTVar' clientsTVar (Map.insert user client)
  putStrLn $ user ++ " login."

  race_ (listenToClient server client) (listenToChannel client)
  atomically $ modifyTVar' clientsTVar (Map.delete user)
  putStrLn $ user ++ " logout."
  
  where
    listenToChannel Client{..} = forever $ do
      msg <- atomically $ readTChan clientSendChan
      sendStr conn msg


listenToClient :: Server -> Client -> IO ()
listenToClient server@Server{..} client@Client{..} = do
  msg <- recvStr clientSocket
  if msg == "logout"
    then return ()
    else do
      case words msg of
        "send":"all":xs -> do
          atomically $ broadcast server (unwords xs)
          putStrLn $ clientName ++ ": " ++ unwords xs

        "send":name:xs -> atomically $ do
          clients <- readTVar clientsTVar
          case Map.lookup name clients of
            Nothing -> writeToChannel ("No user " ++ name ++ " is logged in.") client
            Just c -> writeToChannel (unwords xs) c

        ["who"] -> atomically $ do
          clients <- readTVar clientsTVar
          writeToChannel (intercalate ", " (Map.keys clients)) client

        _ -> atomically $ writeToChannel msg client
      listenToClient server client


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
