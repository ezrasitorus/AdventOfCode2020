import Data.Maybe ( fromJust )

getCode :: String -> (String, Int)
getCode str = (code, sign * read num)
    where
        [code, op:num] = words str
        sign = if op == '+' then 1 else -1

makeProgram :: [(String, Int)] -> [(Int, (String, Int))]
makeProgram = zip [0..]

runProgram :: [Int] -> Int -> Int -> [(Int, (String, Int))] -> Int
runProgram done curr acc prog 
    | elem curr done = acc
    | code == "acc" = runProgram done' (curr + 1) (val+acc) prog
    | code == "jmp" = runProgram done' (curr + val) acc prog
    | code == "nop" = runProgram done' (curr + 1) acc prog
    where
        (code, val) = fromJust $ lookup curr prog
        done' = curr:done


main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ (runProgram [] 0 0 . makeProgram . map getCode . lines) input