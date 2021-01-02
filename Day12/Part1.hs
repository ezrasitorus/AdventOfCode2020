move :: (Char, Int, Int) -> String -> (Char, Int, Int)
move (face, ns, ew) (dir:dis')
    | dir == 'N' || dir == 'S' = (face, ns + dis, ew)
    | dir == 'E' || dir == 'W' = (face, ns, ew + dis)
    | dir == 'F' = move (face, ns, ew) (face:dis')
    | otherwise = (rotate face dir $ read dis', ns, ew)
    where
        dis = dirMap dir * read dis'

dirMap :: Char -> Int
dirMap d
    | d == 'N' || d == 'E' || d == 'F' = 1
    | otherwise = -1

rotate :: Char -> Char -> Int -> Char
rotate face rot deg
    = (!!) dat $ mod (index face dat + ori * mod (div deg 90) 4) 4
    where
        ori = if rot == 'R' then 1 else - 1 
        index :: Char -> String -> Int
        index c = length . takeWhile (/= c)
        dat = "NESW"

main :: IO ()
main = do
    input <- readFile "input.txt"
    let directions = lines input
    print $ foldl move ('E', 0, 0) directions