import Data.List.Split (splitOn)
import Data.List ( intersect )

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

convert :: String -> [[String]]
convert = paragraphs . lines

intersection :: [String] -> String
intersection = foldl1 intersect

-- A potentially quicker way of doing intersection
intersection' :: [String] -> String
intersection' [s] = s
intersection' (s:ss) = filter (\c -> all (elem c) ss) s

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ (sum . map (length . intersection') . convert) input
    