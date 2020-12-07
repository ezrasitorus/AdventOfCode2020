import Data.List.Split ( chunksOf )
import Data.Maybe ( fromJust )
import Data.Bifunctor ( Bifunctor(second) )

getRules :: String -> (String, [(Int, String)])
getRules rule 
    | elem "no" rest = (unwords bag_type, [])
    | otherwise = (unwords bag_type, contained_bags)
    where
        (bag_type, rest) = splitAt 2 $ words rule
        rest_filtered = filter (\s -> (not . isPrefix s) "bag" && s /= "contain") rest
        contained_bags = [((read . head) s, (unwords . tail) s) | s <- chunksOf 3 rest_filtered]

isPrefix :: String -> String -> Bool
isPrefix str = and . zipWith (==) str

canContainShinyGold :: [String] -> [(String, [String])] -> Bool
canContainShinyGold [] _ = False
canContainShinyGold bags rules 
    | elem "shiny gold" bags = True
    | otherwise = any (\x -> canContainShinyGold (fromJust (lookup x rules)) rules) bags

directSizes :: [(String, [(Int, String)])] -> [(String, Int)]
directSizes = map (second (sum . map fst))

getSize :: [(String, [(Int, String)])] -> String -> Int
getSize rules bag
    = sum $ map (\(x,y ) -> x + x * getSize rules y) bag_rules
    where
        bag_rules = fromJust (lookup bag rules)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rules = (map getRules . lines) input
    print $ getSize rules "shiny gold"