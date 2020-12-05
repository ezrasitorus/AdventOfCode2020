findRow :: String -> (Int, Int) -> Int
findRow [] (n, _) = n
findRow (s : ss) (l, u)
  | s == 'F' = findRow ss (l, div (u + l + 1) 2)
  | s == 'B' = findRow ss (div (u + l + 1) 2, u)

findColumn :: String -> (Int, Int) -> Int
findColumn [] (n, _) = n
findColumn (s : ss) (l, u)
  | s == 'L' = findColumn ss (l, div (u + l + 1) 2)
  | s == 'R' = findColumn ss (div (u + l + 1) 2, u)

findSeat :: String -> (Int, Int)
findSeat boardingPass = (findRow row (0, 127), findColumn column (0, 7))
  where
    (row, column) = splitAt 7 boardingPass

findSeatId :: Int -> Int -> Int
findSeatId row column = row * 8 + column

highestSeatId :: [(Int, Int)] -> Int
highestSeatId = maximum . map (uncurry findSeatId)

missingSeats :: [Int] -> [Int]
missingSeats ids = filter (flip notElem ids) [0 .. 127 * 8 + 7]

findMissingMiddleSeat :: [Int] -> Int
findMissingMiddleSeat missingIds = head $ filter (\x -> x /= 0 && notElem (x + 1) missingIds && notElem (x - 1) missingIds) missingIds

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (findMissingMiddleSeat . missingSeats . map (uncurry findSeatId . findSeat) . lines) input