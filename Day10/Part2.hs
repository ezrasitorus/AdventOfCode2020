import Data.Maybe ( isJust, fromJust )
import Data.List ( sort )

getSortedInts :: [String] -> [Int]
getSortedInts = sort . map read

countWays :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
countWays max visited curr
    = (curr, sum possibleWays) : visited
    where
        possibleJumps = (filter (\n -> isJust (lookup n visited) || n == max) . takeWhile (<= max) . take 3) [curr + 1..]
        possibleWays = map (\n -> if n == max then 1 else fromJust $ lookup n visited) possibleJumps

main :: IO ()
main = do
    input <- readFile "input.txt"
    let nums@(max:_) = reverse $ 0 : (getSortedInts . lines) input
    print $ fromJust $ lookup 0 $ foldl (countWays max) [] nums