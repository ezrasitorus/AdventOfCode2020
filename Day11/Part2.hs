import Data.Maybe ( isJust, fromJust )

rowLength :: Int
rowLength = 91

heightLength :: Int 
heightLength = 98

directions :: [Int]
directions = [-rowLength, -rowLength + 1, 1, rowLength + 1, rowLength, rowLength - 1, -1, -rowLength - 1]

getFstDirection :: [(Int, Char)] -> Int -> Int -> [Int]
getFstDirection seats seatNo direction
    | null possibleSeats = []
    | otherwise = [head possibleSeats]
    where
        pos = takeWhile (\x -> div x heightLength >= 0 && div x heightLength <= heightLength - 1 
                        && abs (mod x rowLength - mod (x - direction) rowLength) <= 1) 
                        [seatNo + n * direction | n <- [1..]]
        possibleSeats = filter ((/= Just '.') . flip lookup seats) pos
    
getAdjSeatInfo :: [(Int, Char)] -> (Int, Char) -> (Int, Char)
getAdjSeatInfo seats (seatNo, seatType) 
    | seatType == 'L' && occSeatsNo == 0 = (seatNo, '#') 
    | seatType == '#' && occSeatsNo >= 5 = (seatNo, 'L')
    | otherwise = (seatNo, seatType)
    where
        occSeatsNo = length $ filter (== Just '#') $ map (`lookup` seats) $ concatMap (getFstDirection seats seatNo) directions

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