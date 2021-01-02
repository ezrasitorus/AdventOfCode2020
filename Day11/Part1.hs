import Data.Maybe ( isJust, fromJust )

rowLength :: Int
rowLength = 91

getAdjSeatNums :: Int -> [Int]
getAdjSeatNums n
    = map (n +) $ [-rowLength, rowLength] ++ left ++ right 
    where
        blank = [-rowLength, 0, rowLength]
        left = if mod n rowLength /= 0 then map (subtract 1) blank else []
        right = if mod n rowLength /= rowLength - 1 then map (+ 1) blank else []
    
getAdjSeatInfo :: [(Int, Char)] -> (Int, Char) -> (Int, Char)
getAdjSeatInfo seats (seatNo, seatType) 
    | seatType == 'L' && occSeatsNo == 0 = (seatNo, '#') 
    | seatType == '#' && occSeatsNo >= 4 = (seatNo, 'L')
    | otherwise = (seatNo, seatType)
    where
        adjSeatTypes = map fromJust $ filter isJust $ map (`lookup` seats) $ getAdjSeatNums seatNo
        occSeatsNo = length $ filter (== '#') adjSeatTypes

getNextSeats :: [(Int, Char)] -> [(Int, Char)]
getNextSeats seats = map (getAdjSeatInfo seats) seats

iterateUntilDone :: [(Int, Char)] -> [(Int, Char)] 
iterateUntilDone seats
    | getNextSeats seats == seats = seats
    | otherwise = iterateUntilDone (getNextSeats seats)

countIterations :: [(Int, Char)] -> Int -> Int 
countIterations seats acc
    | getNextSeats seats == seats = acc 
    | otherwise = countIterations (getNextSeats seats) (acc + 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let seatsInfo = zip [0..] $ (concat . lines) input
    let res = iterateUntilDone seatsInfo
    print $ countIterations seatsInfo 0
    print $ (length . filter (== '#') . map snd) res