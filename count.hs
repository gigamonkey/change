import Data.Maybe
import qualified Data.Map.Strict as M

-- The reason the naive approach gets so slow is that the same
-- problems get computed over and over. In fact to compute the number
-- of ways to make change for $1.00 with the US coins, ways will be
-- called 38,901 times with a 165 combinations being called 100 times
-- or more. And it grows fast: to compute $1.10 it takes 53,301
-- invocations.

type Counts = M.Map ([Int], Int) (Int, Int)

us :: Num a => [a]
us = [1,5,10,25,50]

ways :: [Int] -> Int -> Counts -> (Integer, Counts)
ways [] n m         = (0, up ([], n) m 1 0)
ways cs 0 m         = (1, up (cs, 0) m 1 0)
ways cs n m | n < 0 = (0, up (cs, n) m 1 0)
ways (c:cs) n m     = (x' + x'', up (c:cs, n) m'' 1 0)
    where
      recur c' n' m = addBelow (c:cs, n) m (c', n') $ ways c' n' m
      (x', m')      = recur (c:cs) (n - c) m
      (x'', m'')    = recur cs n m'

addCounts :: Num a => (a, a) -> (a, a) -> (a, a)
addCounts (a, b) (a', b') = (a + a', b + b')

up k m a b = M.insertWith addCounts k (a, b) m

below k m' m = 1 + snd (fromJust $ M.lookup k m') - maybe 0 snd (M.lookup k m)

addBelow k m k' (x, m') = (x, up k m' 0 (below k' m' m))

main :: IO ()
main = do
  let (x, m) = ways us 100 M.empty
  print x
  print $ M.lookup (us, 100) m
