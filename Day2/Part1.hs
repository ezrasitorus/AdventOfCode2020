import Data.List.Split

parseString :: String -> [String]
parseString = words . map (\c -> if c == ':' || c == '-' then ' ' else c)

check :: [String] -> Bool
check [s,e,[c],str] = size >= s' && size <= e'
    where
        size = length $ filter (== c) str
        [s', e'] = map read [s, e]

count :: String -> Int
count str = length $ filter check $ map parseString $ lines str

main :: IO ()
main = do
    passwords <- readFile "input.txt"
    print $ count passwords