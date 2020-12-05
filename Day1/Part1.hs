convert :: String -> [Int]
convert = map read . words

getResult :: [Int] -> Int
getResult [] = error "Values not found"
getResult (n : ns)
  | complement `elem` ns = complement * n
  | otherwise = getResult ns
  where
    complement = 2020 - n

main :: IO ()
main = do
  myFile <- readFile "input.txt"
  print $ (getResult . convert) myFile