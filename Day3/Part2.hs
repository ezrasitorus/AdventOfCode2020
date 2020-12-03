positions :: Int -> [Int]
positions right = [mod (right * n) 31| n <- [0..]]

spots :: Int -> [String] -> [Int] -> String
spots down paths = zipWith (!!) [paths !! (n * down) | n <- [0..div (length paths) down - 1]]

numOfTrees :: String -> Int
numOfTrees = length . filter (== '#')

paths :: [(Int, Int)]
paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

treesOnPaths :: [String] -> [Int]
treesOnPaths mapOfPlace = map (\(right, down) -> numOfTrees $ spots down mapOfPlace $ positions right) paths

main :: IO ()
main = do
    input <- readFile "input.txt"
    let map = lines input
    print $ (product . treesOnPaths) map