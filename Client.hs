import Network.Socket
    -- ( getAddrInfo,
    --   connect,
    --   socket,
    --   close,
    --   defaultProtocol,
    --   AddrInfo(addrAddress),
    --   Family(AF_INET),
    --   Socket,
    --   SocketType(Stream) )
import Network.Socket.ByteString ( recv, send )
import qualified Data.ByteString.UTF8 as U
import Text.StringRandom (stringRandomIO)
import Data.Text (unpack, pack)

keyLength :: Int
keyLength = 16


main :: IO ()
main = do
  key <- unpack <$> stringRandomIO (pack (".{" ++ show keyLength ++ "}"))
  putStrLn "My chat room client. Version One."
  sock <- getSock
  runClient sock


runClient :: Socket-> IO ()
runClient sock = do
  input <- getLine
  send sock (U.fromString input)
  recv sock 4096 >>= (putStrLn . U.toString)
  runClient sock 




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