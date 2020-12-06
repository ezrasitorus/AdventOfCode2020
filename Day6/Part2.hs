import Data.List.Split (splitOn)
import Data.List ( intersect )

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

convert :: String -> [[String]]
convert = paragraphs . lines

intersection :: [String] -> String
intersection = foldl1 intersect

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ (sum . map (length . intersection) . convert) input
    