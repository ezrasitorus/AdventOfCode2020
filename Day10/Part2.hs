import Data.Maybe ( isJust, fromJust )
import Data.List ( sort )

getSortedInts :: [String] -> [Int]
getSortedInts = sort . map read

diffCounter :: [Int] -> Int -> Int
diffCounter as  diff = length $ filter (\(x, y) -> y - x == diff) $ zip as (tail as)

countWays :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
countWays max visited curr
    = (curr, sum possibleWays) : visited
    where
        possibleJumps = (filter (\n -> isJust (lookup n visited) || n == max) . takeWhile (<= max) . take 3) [curr + 1..]
        possibleWays = map (\n -> if n == max then 1 else fromJust $ lookup n visited) possibleJumps

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sortedNums = (getSortedInts . lines) input
    let rev = reverse $ 0 : sortedNums
    let max = head rev
    let memo = foldl (countWays max) [] rev
    print $ fromJust $ lookup 0 memo