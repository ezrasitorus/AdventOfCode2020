move :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
move (ns, ew, ns_wp, ew_wp) (dir:dis')
    | dir == 'N' || dir == 'S' = (ns, ew, ns_wp + dis, ew_wp)
    | dir == 'E' || dir == 'W' = (ns, ew, ns_wp, ew_wp + dis)
    | dir == 'F' = (ns + dis * ns_wp, ew + dis * ew_wp, ns_wp, ew_wp)
    | otherwise = (ns, ew, ns_wp', ew_wp')
    where
        dis = dirMap dir * read dis'
        (ns_wp', ew_wp') = rotate ns_wp ew_wp dir (read dis')

dirMap :: Char -> Int
dirMap d
    | d == 'N' || d == 'E' || d == 'F' = 1
    | otherwise = -1

rotate :: Int -> Int -> Char -> Int -> (Int, Int)
rotate ns_wp ew_wp rot deg
    = rotate' ns_wp ew_wp count
    where
        num = mod (div deg 90) 4 
        count = if rot == 'R' then num else 4 - num 
        rotate' :: Int -> Int -> Int -> (Int, Int)
        rotate' a b 0 = (a, b)
        rotate' a b n = rotate' (-b) a (n-1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let directions = lines input
    print $ foldl move (0, 0, 1, 10) directions