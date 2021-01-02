import Data.List.Split ( splitOn )
import Data.Char ( isDigit )
import Data.List ( sortBy )

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f asbs = zip (map fst asbs) (map (f . snd) asbs)

getTimes :: String -> [(Int, Int)]
getTimes input =
                 (mapSnd read . filter (\(_, b) -> b /= []) . mapSnd (filter isDigit)) $ 
                (zip [0,-1..] . splitOn ",") rest
    where
        [earliest_str, rest] = lines input

computeCoeff ::  Int -> Int -> (Int, Int)
computeCoeff a b = go a b 1 0 0 1
     where go a b s0 s1 t0 t1 | r==0 = (s1,t1)
                              | otherwise = go b r s1 s t1 t
                              where (q,r) = quotRem a b
                                    (s,t) = (s0-s1*q, t0-t1*q)

crt :: [(Int, Int)] -> Int 
crt buses
    = mod (sum $ zipWith3 (\x y z -> x * y * z) (map fst buses) ys zs) n
        where
            k = length buses - 1
            n = (product . map snd) buses
            ys = [div n b | (_, b) <- buses]
            zs = [fst $ computeCoeff (ys !! i) (map snd buses !! i) | i <- [0..k]]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let times = getTimes input
    print times
    print $ crt times