solve :: [Int] -> [([Int], Int)]
solve nums = filter (not . uncurry isValid) [ (ns, val)  | n <- [0..length nums - 26], 
                                            let (ns, [val]) = (splitAt 25 . take 26 . drop n) nums]

isValid :: [Int] -> Int -> Bool
isValid [_] _ = False
isValid (n:ns) val
    | elem (val - n) ns = True
    | otherwise = isValid ns val

solve2 :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
solve2 nums i invalid acc (n:ns)
    | curr > invalid = solve2 nums (i + 1) invalid [] (drop i nums)
    | curr + n == invalid = n:acc
    | otherwise = solve2 nums i invalid (n:acc) ns  
    where 
        curr = sum acc 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let nums = (map read . lines) input
    let invalid = (head . map snd . solve) nums
    print invalid
    let sol2 = solve2 nums 0 invalid [] nums 
    print $ minimum sol2 + maximum sol2