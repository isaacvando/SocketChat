-- {-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import Network.Socket.ByteString
import Data.List.Split (splitOn)

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10746")
  print $ addrAddress hostAddr
  bind sock (addrAddress hostAddr)
  listen sock 1
  loop sock

loop :: Socket -> IO ()
loop sock = do
  (sock', _) <- accept sock
  send sock' "foobarbaz\n"
  close sock'
  loop sock

parseUsers :: String -> [(String, String)]
parseUsers xs = map f (lines xs)
  where 
    f x = (head splits, concat (tail splits))
      where
        trimmed = (init . tail) x 
        splits = splitOn ", " trimmed
