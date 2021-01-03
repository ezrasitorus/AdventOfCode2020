import Data.List () 
import Data.Maybe ( fromJust, isNothing )

starting :: [(Int, Int)]
starting = zip [14,8,16,0,1,17] [1..]

getNext :: (Int, Int, [(Int, Int)]) -> (Int, Int, [(Int, Int)])
getNext (time, recent, nums)
    | isNothing res = (time + 1, 0, (recent, time) : nums)
    | otherwise = (time + 1, time - fromJust res, (recent, time) : filter ((/= recent) . fst) nums)
    where
        res = lookup recent nums

getNextWithCount :: Int -> (Int, Int, [(Int, Int)]) -> (Int, Int, [(Int, Int)])
getNextWithCount c a@(t,_,_)
    | c == t = a
    | otherwise = getNextWithCount c (getNext a)

main :: IO ()
main = do
    let (_, res, _) = getNextWithCount 30000000 (7, 0, starting)
    print res

test :: Int -> String
test 30000000 = "Hello"
test n = test (succ n)