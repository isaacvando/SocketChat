import Network.Socket
import Network.Socket.ByteString ( recv, send )
import qualified Data.ByteString.UTF8 as U
import Control.Concurrent.Async (race_)
import Control.Monad


main :: IO ()
main = do
  putStrLn "My chat room client. Version Two."
  sock <- getSock
  race_ (sendMsg sock) (recvMsg sock)
  close sock

sendMsg :: Socket -> IO ()
sendMsg sock = do
  input <- getLine
  if input == "logout"
    then return ()
    else do
      send sock (U.fromString input)
      sendMsg sock

recvMsg :: Socket -> IO ()
recvMsg sock = do
  msg <- U.toString <$> recv sock 4096
  -- unless (null msg) (putStrLn msg)
  putStrLn msg
  recvMsg sock






talk :: Socket -> String -> String -> IO ()
talk sock key msg = do
  _ <- send sock (U.fromString (key ++ msg))
  recv sock 4096 >>= (putStrLn . U.toString)

getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock


  -- let go = talk sock key input >> runClient sock key
  -- case words input of
  --   ["logout"] -> talk sock key input
  --   "send":_ -> go
  --   ["newuser", _, _] -> go
  --   ["login", _, _] -> go
  --   xs -> putStrLn ("\"" ++ unwords xs ++ "\" is not a valid command.") >> runClient sock key