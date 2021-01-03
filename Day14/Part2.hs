import Data.Char
import Data.Bits
import Data.Maybe

getMask :: String -> String 
getMask = drop 7 

addToMemory :: [(Int, Int)] -> String -> String -> [(Int, Int)] 
addToMemory mem mask msg
    = foldl (\m a -> add (a, val) m) mem addresses
    where
        val = (read . filter isDigit . dropWhile (/= '=')) msg
        address :: Int
        address = (read . takeWhile isDigit . drop 4) msg
        mask' =  [if x == 'X' || x == '1' 
                  then x
                  else (head . show) $ shiftR address i .&. 1
                  | (i, x) <- zip [35,34..] mask]
        addresses = foldl (\as x -> if isDigit x 
                                    then map ((+) (read [x]) . (*) 2) as
                                    else map ((+) 1 . (*) 2) as ++ map (* 2) as)
                    [0] mask'

add :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
add (a, v) mem
    | isNothing $ lookup a mem = (a, v) : mem
    | otherwise = (a, v) : filter (\(x, _) -> x /= a) mem

executeCmd :: (String, [(Int, Int)]) -> String -> (String, [(Int, Int)])
executeCmd (mask, mem) msg
    | msg !! 1 == 'a' = (getMask msg, mem)
    | otherwise = (mask, addToMemory mem mask msg)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let cmds = lines input
    let (_, mem) = foldl executeCmd ([], []) cmds
    print $ (sum . map snd) mem
