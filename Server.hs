import Network.Socket
import Data.List.Split (splitOn)

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  sock <- socket AF_INET Stream defaultProtocol
  hostAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") Nothing
  bind sock (addrAddress hostAddr)
  print sock
  close sock

parseUsers :: String -> [(String, String)]
parseUsers xs = map f (lines xs)
  where 
    f x = (head splits, concat (tail splits))
      where
        trimmed = (init . tail) x 
        splits = splitOn ", " trimmed
