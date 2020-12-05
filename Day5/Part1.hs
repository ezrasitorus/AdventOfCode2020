findRow :: String -> (Int, Int) -> Int
findRow [] (n, _) = n
findRow (s : ss) (l, u)
  | s == 'F' = findRow ss (l, div (u + l + 1) 2)
  | s == 'B' = findRow ss (div (u + l + 1) 2, u)

findColumn :: String -> (Int, Int) -> Int
findColumn [] (n, _) = n
findColumn ('L' : ss) (l, u) = findColumn ss (l, div (u + l + 1) 2)
findColumn ('R' : ss) (l, u) = findColumn ss (div (u + l + 1) 2, u)

findSeat :: String -> (Int, Int)
findSeat boardingPass = (findRow row (0, 127), findColumn column (0, 7))
  where
    (row, column) = splitAt 7 boardingPass

findSeatId :: Int -> Int -> Int
findSeatId row column = row * 8 + column

highestSeatId :: [(Int, Int)] -> Int
highestSeatId = maximum . map (uncurry findSeatId)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (highestSeatId . map findSeat . lines) input