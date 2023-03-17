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
import Control.Exception



data Server = Server
  {
    clientsTVar :: TVar (Map.Map String Client)
    , usersTVar :: TVar [(String,String)]
    , listener :: Socket
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
  bracket 
    newServer 
    finish
    go

  where 
    go server@Server{..} = forever $ do
      (conn,_) <- accept listener
      forkFinally (runClient server conn) (\_ -> close conn)

    finish Server{..} = do
      users <- readTVarIO usersTVar
      writeFile "users.txt" (renderUsers users)


newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  users <- parseUsers <$> S.readFile "users.txt"
  u <- newTVarIO users
  s <- getSock
  return Server { clientsTVar = c, usersTVar = u, listener = s}


newClient :: Socket -> String -> IO Client
newClient conn name = do
  tchan <- newTChanIO
  return Client {clientSocket = conn, clientSendChan = tchan, clientName = name}


runClient :: Server -> Socket -> IO ()
runClient server@Server{..} conn = do
  client@Client{..} <- login server conn
  atomically $ modifyTVar' clientsTVar (Map.insert clientName client)
  putStrLn $ clientName ++ " login."

  race_ (listenToClient server client) (listenToChannel client)
  atomically $ modifyTVar' clientsTVar (Map.delete clientName)
  putStrLn $ clientName ++ " logout."
  
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
          let msg = clientName ++ ": " ++ unwords xs
          atomically $ broadcast server msg
          putStrLn msg

        "send":name:xs -> do
          ok <- atomically $ do
            clients <- readTVar clientsTVar
            case Map.lookup name clients of
              Nothing -> writeToChannel ("No user " ++ name ++ " is logged in.") client >> return False
              Just c -> writeToChannel (unwords xs) c >> return True
          when ok (putStrLn $ clientName ++ " (to " ++ name ++ "): " ++ unwords xs)

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


login :: Server -> Socket -> IO Client
login server@Server{..} conn = do
  msg <- recvStr conn


  case words msg of
    ["login", user, pass] -> do
      ok <- atomically $ do
        users <- readTVar usersTVar
        return $ (user, pass) `elem` users
      if ok
        then newClient conn user
        else retry "Denied. User name or password incorrect."


    ["newuser", user, pass] 
      | length user < 3 || length user > 32 
        -> retry "Username must be between 3 and 32 characters long"

      | length pass < 4 || length pass > 8 
        -> retry "Password must be between 4 and 8 characters long"

      | otherwise 
        -> do
          taken <- atomically $ do
            users <- readTVar usersTVar
            if user `elem` map fst users
              then return True
              else writeTVar usersTVar ((user,pass):users) >> return False
          if taken 
            then 
              retry $ "The username \"" ++ user ++ "\" is already taken."
            else do
              putStrLn "New user account created."
              retry "New user account created. Please login."

    _ -> retry "Denied. Please login first."

  where
    retry msg = sendStr conn msg >> login server conn


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

renderUsers :: [(String,String)] -> String
renderUsers = unlines . map go
  where
    go (name,pass) = "(" ++ name ++ ", " ++ pass ++ ")"
