import Data.List ( elemIndex )
import Data.Maybe ( fromJust, isNothing )

starting :: [Int]
starting = [14,8,16,0,1,17]

getNext :: [Int] -> [Int]
getNext n'
    | isNothing res = n' ++ [0]
    | otherwise = n' ++ [fromJust res + 1]
    where
        (n:ns) = reverse n'
        res = elemIndex n ns

countIterate :: Int -> [Int] -> [Int]
countIterate 0 ns = ns
countIterate c ns = countIterate (c - 1) (getNext ns)

main :: IO ()
main = do
    let elfSeq = countIterate (2020 - 6) starting
    print $ elfSeq !! 2019
