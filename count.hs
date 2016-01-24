import Data.Maybe
import qualified Data.Map.Strict as M

-- The reason the naive approach gets so slow is that the same
-- problems get computed over and over. In fact to compute the number
-- of ways to make change for $1.00 with the US coins, ways will be
-- called 38,901 times with a 165 combinations being called 100 times
-- or more. And it grows fast: to compute $1.10 it takes 53,301
-- invocations.

us :: Num a => [a]
us = [1,5,10,25,50]

type Counts = M.Map ([Int], Int) (Int, Int)

upcounts :: (Int, Int) -> (Int, Int) -> (Int, Int)
upcounts (a, b) (a', b') = (a + a', b + b')

cWays :: [Int] -> Int -> Counts -> (Integer, Counts)
cWays [] n m = (0, M.insertWith upcounts ([], n) (1, 0) m)
cWays cs 0 m = (1, M.insertWith upcounts (cs, 0) (1, 0) m)
cWays (c:cs) n m
    | n < 0 = (0, M.insertWith upcounts (c:cs, n) (1, 0) m)
    | otherwise = (x' + x'', updated)
    where
      (x', m')   = cWays (c:cs) (n - c) m
      (x'', m'') = cWays cs n m'
      belowLeft  = snd (fromJust $ M.lookup (c:cs, n - c) m') - maybe 0 snd (M.lookup (c:cs, n - c) m)
      belowRight = snd (fromJust $ M.lookup (cs, n) m'') - maybe 0 snd (M.lookup (cs, n) m')
      updated    = M.insertWith upcounts (c:cs, n) (1,  2 + belowLeft + belowRight) m''

main :: IO ()
main = do
  let (x, m) = cWays us 100 M.empty
  print x
  print $ M.lookup (us, 100) m
