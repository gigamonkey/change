import Control.Monad.State
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as M

-- The reason the naive approach gets so slow is that the same
-- problems get computed over and over. In fact to compute the number
-- of ways to make change for $1.00 with the US coins, ways will be
-- called 38,901 times with a 165 combinations being called 100 times
-- or more. And it grows fast: to compute $1.10 it takes 53,301
-- invocations.

type Key = ([Int], Int)
type Counts = M.Map Key (Int, Int)

us :: Num a => [a]
us = [1,5,10,25,50]

ways :: [Int] -> Int -> Integer
ways coins amount = evalState (ways' coins amount) M.empty

counts :: [Int] -> Int -> Counts
counts coins amount = execState (ways' coins amount) M.empty

ways' :: [Int] -> Int -> State Counts Integer
ways' [] n         = incr ([], n) $ return 0
ways' cs 0         = incr (cs, 0) $ return 1
ways' cs n | n < 0 = incr (cs, n) $ return 0
ways' (c:cs) n     = incr (c:cs, n) $ (+) <$> recur cs n <*> recur (c:cs) (n - c)
    where
      recur cs' n' = get >>= \m -> mapState (addBelow (c:cs, n) m (cs', n')) (ways' cs' n')

incr :: Key -> State Counts a -> State Counts a
incr k = withState $ M.insertWith addCounts k (1, 0)

addCounts :: Num a => (a, a) -> (a, a) -> (a, a)
addCounts (a, b) (a', b') = (a + a', b + b')

addBelow :: Key -> Counts -> Key -> (Integer, Counts) -> (Integer, Counts)
addBelow k m k' (x, m') = (x, M.insertWith addCounts k (0, below k' m' m) m')

below :: (Num a, Ord k) => k -> M.Map k (a1, a) -> M.Map k (a2, a) -> a
below k m' m = 1 + snd (fromJust (M.lookup k m')) - maybe 0 snd (M.lookup k m)

main :: IO ()
main = do
  --let w = ways us 100
  let c = counts us 100
  --mapM_ print $ sortBy (\((a, b), _) ((c, d), _) -> if a == c then compare d b else compare a c) [ (b, a) | (a, b) <- M.assocs c ]
  mapM_ print $ reverse $ sortBy (comparing (snd . fst)) [ (b, a) | (a, b) <- M.assocs c ]
  -- print w
  -- print $ M.lookup (us, 100) c
  -- print $ M.lookup ([], 100) c
  -- print $ M.lookup (us, 0) c
  -- print $ M.lookup (drop 1 us, 0) c
  -- print $ M.lookup (drop 2 us, 0) c
  -- print $ M.lookup (drop 3 us, 0) c
  -- print $ M.lookup (drop 4 us, 0) c
  -- print $ M.lookup (drop 5 us, 0) c
