import Data.Maybe

getCode :: String -> (String, Int)
getCode str = (code, sign * read num)
    where
        [code, op:num] = words str
        sign = if op == '+' then 1 else -1

makeProgram :: [(String, Int)] -> [(Int, (String, Int))]
makeProgram = zip [0..]

runProgramAndHalt :: [Int] -> Int -> Int -> [(Int, (String, Int))] -> Int
runProgramAndHalt done curr acc prog 
    | elem curr done = acc
    | code == "acc" = runProgramAndHalt done' (curr + 1) (val+acc) prog
    | code == "jmp" = runProgramAndHalt done' (curr + val) acc prog
    | code == "nop" = runProgramAndHalt done' (curr + 1) acc prog
    where
        (code, val) = fromJust $ lookup curr prog
        done' = curr:done

runProgram :: [Int] -> Int -> Int -> [(Int, (String, Int))] -> Maybe Int
runProgram done curr acc prog
    | elem curr done = Nothing
    | curr == length prog = Just acc
    | code == "acc" = runProgram done' (curr + 1) (val+acc) prog
    | code == "jmp" = runProgram done' (curr + val) acc prog
    | code == "nop" = runProgram done' (curr + 1) acc prog
    where
        (code, val) = fromJust $ lookup curr prog
        done' = curr:done

swap :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
swap abs a b
    = map (\(x,y) -> if x == a then (a, b) else (x, y)) abs

runPossiblePrograms :: [(Int, (String, Int))] -> [Maybe Int]
runPossiblePrograms prog = [runProgram [] 0 0 $ if code == "nop" then swap prog n ("jmp", val) else 
                                                if code == "jmp" then swap prog n ("nop", val)
                                                else prog | (n, (code, val)) <- prog]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ (fromJust . head . filter isJust . runPossiblePrograms . makeProgram . map getCode . lines) input