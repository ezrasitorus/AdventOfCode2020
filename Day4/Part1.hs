import Data.List.Split ( splitOn )

required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: [String] -> Bool
isValid passport = all (`elem` passport) required

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""] 

getPassportField :: String -> String
getPassportField (':':_) = []
getPassportField (s:ss) = s : getPassportField ss

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length $ filter isValid $ (map (map getPassportField . concatMap words) . paragraphs . lines) input