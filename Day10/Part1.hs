import Data.List ( sort )

getSortedInts :: [String] -> [Int]
getSortedInts = sort . map read

diffCounter :: [Int] -> Int -> Int
diffCounter as  diff = length $ filter (\(x, y) -> y - x == diff) $ zip as (tail as)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sortedNums = (getSortedInts . lines) input
    print $ (product . map (succ . diffCounter sortedNums)) [1, 3]