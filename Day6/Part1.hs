import Data.List.Split (splitOn)
import Data.List ( group, sort ) 

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

convert :: String -> [String]
convert = map concat . paragraphs . lines

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ (sum . map (length . group . sort) . convert) input
    