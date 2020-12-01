import Data.Maybe

convert :: String -> [Int]
convert = map read . words

getResult :: [Int] -> Int
getResult [] = error "Values not found"
getResult (n:ns)
    | isNothing res = getResult ns
    | otherwise = n * fromJust res
    where
        complement = 2020 - n
        res = getResult' ns complement

getResult' :: [Int] -> Int -> Maybe Int
getResult' [] _ = Nothing
getResult' [n] _ = Nothing
getResult' (n:ns) c
    | (c - n) `elem` ns = Just (n * (c - n))
    | otherwise = getResult' ns c


main :: IO ()
main = do
    myFile <- readFile "input.txt"
    print $ (getResult . convert) myFile