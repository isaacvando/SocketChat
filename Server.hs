import Network.Socket
import Data.List.Split (splitOn)

main :: IO ()
main = do
  users <- parseUsers <$> readFile "users.txt"
  print users

parseUsers :: String -> [(String, String)]
parseUsers xs = map f (lines xs)
  where 
    f x = (head splits, concat (tail splits))
      where
        trimmed = (init . tail) x 
        splits = splitOn ", " trimmed
