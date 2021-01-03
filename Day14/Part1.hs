import Data.Char ( isDigit )
import Data.Bits ( Bits(setBit, clearBit) )
import Data.Maybe ( isNothing )

getMask :: String -> [(Int, Int)]
getMask msg = zip (map fst ns)  ns2
        where
            mask = drop 7 msg
            ns = (filter ((/= 'X') . snd) . zip [35,34..]) mask
            ns2 = map (\(_, n) -> read [n]) ns 

addToMemory :: [(Int, Int)] -> [(Int, Int)] -> String -> [(Int, Int)] 
addToMemory mem mask msg
    = add (address, actVal) mem
    where
        address = (read . takeWhile isDigit . drop 4) msg
        val = (read . filter isDigit . dropWhile (/= '=')) msg
        actVal = foldl (\a (i, d) -> if d == 0 then clearBit a i else setBit a i) val mask

add :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
add (a, v) mem
    | isNothing $ lookup a mem = (a, v) : mem
    | otherwise = (a, v) : filter (\(x, _) -> x /= a) mem

executeCmd :: ([(Int, Int)], [(Int, Int)]) -> String -> ([(Int, Int)], [(Int, Int)])
executeCmd (mask, mem) msg
    | msg !! 1 == 'a' = (getMask msg, mem)
    | otherwise = (mask, addToMemory mem mask msg)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let cmds = lines input
    let (_, mem) = foldl executeCmd ([], []) cmds
    print $ (sum . map snd) mem
