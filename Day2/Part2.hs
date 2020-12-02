import Data.List.Split

parseString :: String -> [String]
parseString = words . map (\c -> if c == ':' || c == '-' then ' ' else c)

check :: [String] -> Bool
check [s,e,[c],str] 
    | length str < e' = False
    | otherwise = (str !! (s' - 1) == c) /= (str !! (e' - 1) == c)
    where
        [s', e'] = map read [s, e]

count :: String -> Int
count str = length $ filter check $ map parseString $ lines str

main :: IO ()
main = do
    passwords <- readFile "input.txt"
    print $ count passwords