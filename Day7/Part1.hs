import Data.Char ( isNumber )
import Data.Maybe ( fromJust )

getRules :: String -> (String, [String])
getRules rule 
    | elem "no" rest = (unwords bag_type, [])
    | otherwise = (unwords bag_type, contained_bags)
    where
        (bag_type, rest) = splitAt 2 $ words rule
        rest_filtered = filter (\s -> (not . isPrefix s) "bag" && (not . isNumber . head) s && s /= "contain") rest
        contained_bags = [(unwords . take 2 . drop (2 * i)) rest_filtered | i <- [0..div (length rest_filtered) 2 - 1]]

isPrefix :: String -> String -> Bool
isPrefix str = and . zipWith (==) str

canContainShinyGold :: [String] -> [(String, [String])] -> Bool
canContainShinyGold [] _ = False
canContainShinyGold bags rules 
    | elem "shiny gold" bags = True
    | otherwise = any (\x -> canContainShinyGold (fromJust (lookup x rules)) rules) bags

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = (map getRules . lines) input
    print $ length $ filter (\(_, cs) -> canContainShinyGold cs rules) rules