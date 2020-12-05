positions :: [Int]
positions = [mod (3 * n) 31 | n <- [0 ..]]

spots :: [String] -> [Int] -> String
spots = zipWith (!!)

numOfTrees :: String -> Int
numOfTrees = length . filter (== '#')

main :: IO ()
main = do
  input <- readFile "input.txt"
  let map = lines input
  print $ numOfTrees $ spots map positions