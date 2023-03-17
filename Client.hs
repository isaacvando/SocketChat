-- Isaac Van Doren
-- 3/16/23
-- Multithreaded chat client


import Network.Socket
import Network.Socket.ByteString ( recv, send )
import qualified Data.ByteString.UTF8 as U
import Control.Concurrent.Async (race_)
import Control.Exception (bracket)


main :: IO ()
main = do
  putStrLn "My chat room client. Version Two."
  bracket 
    getSock 
    (\s -> send s (U.fromString "logout") >> close s)
    (\s -> race_ (sendLoop s) (recvLoop s))


sendLoop :: Socket -> IO ()
sendLoop sock = do
  input <- getLine
  case words input of
    ["logout"] -> return ()

    "send":xs 
      | length (unwords xs) > 256 || null (unwords xs)
        -> retry "The message must be between 1 and 256 characters in length."
      | otherwise
        -> talk input


    ["newuser", user, pass]
      | length user < 3 || length user > 32 
        -> retry "Username must be between 3 and 32 characters long"

      | length pass < 4 || length pass > 8 
        -> retry "Password must be between 4 and 8 characters long"

      | otherwise 
        -> talk input

    ["login", _, _] -> talk input

    ["who"] -> talk input
    _ -> retry ("\"" ++ input ++ "\" is not a valid command.")

  where
    talk s = send sock (U.fromString s) >> sendLoop sock
    retry msg = putStrLn msg >> sendLoop sock


recvLoop :: Socket -> IO ()
recvLoop sock = do
  msg <- U.toString <$> recv sock 4096
  putStrLn msg
  recvLoop sock


getSock :: IO Socket
getSock = do
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  connect sock (addrAddress hostAddr)
  return sock
