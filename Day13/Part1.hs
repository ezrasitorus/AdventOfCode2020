import Data.List.Split ( splitOn )
import Data.Char ( isDigit )
import Data.List ( sortBy )

getTimes :: String -> (Int, [Int])
getTimes input = (read earliest_str, (map (read . filter isDigit) . filter (/= []) . splitOn ",x") rest)
    where
        [earliest_str, rest] = lines input

solve :: Int -> [Int] -> Int 
solve earliest buses
    = bus_id * (time - earliest)
        where
        ((bus_id, time):ts) = sortBy (\x y -> compare (snd x) (snd y)) $ 
                zip buses [(head . dropWhile (<= earliest)) [n * b | n <- [0..]] | b <- buses]
        

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (earliest, times) = getTimes input
    print $ uncurry solve (earliest, times)