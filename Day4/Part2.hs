import Data.Char (isNumber)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

byrValid :: String -> Bool
byrValid byr = read byr >= 1920 && read byr <= 2002

iyrValid :: String -> Bool
iyrValid iyr = read iyr >= 2010 && read iyr <= 2020

eyrValid :: String -> Bool
eyrValid eyr = read eyr >= 2020 && read eyr <= 2030

hgtValid :: String -> Bool
hgtValid hgt
  | elem 'c' hgt = hgt_num >= 150 && hgt_num <= 193
  | otherwise = hgt_num >= 59 && hgt_num <= 76
  where
    hgt_num = read $ takeWhile isNumber hgt

hclValid :: String -> Bool
hclValid ('#' : hcl) = length hcl == 6 && all (\x -> elem x "abcdef" || isNumber x) hcl
hclValid _ = False

eclValid :: String -> Bool
eclValid = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pidValid :: String -> Bool
pidValid pid = (9 == length pid) && all isNumber pid

cidValid :: String -> Bool
cidValid = const True

validityChecks :: [(String, String -> Bool)]
validityChecks = [("byr", byrValid), ("iyr", iyrValid), ("eyr", eyrValid), ("hgt", hgtValid), ("hcl", hclValid), ("ecl", eclValid), ("pid", pidValid), ("cid", cidValid)]

potentiallyValid :: [(String, String)] -> Bool
potentiallyValid passport = all (`elem` map fst passport) required

isValid :: [(String, String)] -> Bool
isValid = all (\(f, d) -> (fromJust $ lookup f validityChecks) d)

paragraphs :: [String] -> [[String]]
paragraphs = splitOn [""]

getPassportField :: String -> (String, String)
getPassportField s = (f, d)
  where
    [f, d] = splitOn ":" s

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ length $ filter (\x -> isValid x && potentiallyValid x) $ (map (map getPassportField . concatMap words) . paragraphs . lines) input