import Control.DeepSeq
import Control.Exception
import Control.Monad.State
import Criterion.Main
import Debug.Trace
import System.CPUTime
import Text.Printf
import qualified Data.Map.Strict as M

us :: Num a => [a]
us = [1,5,10,25,50]

-- Relatively simple recursive solution: the number of ways to make
-- change with a given set of coins is all the ways you can make
-- change with at least one of the first coin plus all the ways you
-- can make change with none of the first coin. The first of those is
-- same as the number of ways you can make change for the original
-- amount minus the denomination of the first coin with all the coins.
-- Note that this function is very slow. It's fast enough to compute
-- the ways to compute change for a dollar using the standard us coins
-- up to half-dollar but for larger amounts it quickly bogs down.

ways [] _     = 0
ways _ 0      = 1
ways (c:cs) n = if n < 0 then 0 else ways (c:cs) (n - c) + ways cs n

-- The reason this naive approach gets so slow is that the same
-- problems get computed over and over. In fact to compute the number
-- of ways to make change for $1.00 with the US coins, ways will be
-- called 38,901 times with a 165 combinations being called 100 times
-- or more. And it grows fast: to compute $1.10 it takes 53,301
-- invocations.

tWays :: [Int] -> Int -> Integer
tWays [] n     = trace (show (([] :: [Int]), n)) 0
tWays cs 0     = trace (show (cs, 0)) 1
tWays (c:cs) n = trace (show ((c:cs), n)) (if n < 0 then 0 else tWays (c:cs) (n - c) + tWays cs n)

fib = (map f [0 .. ] !!) where
    f 0 = 0
    f 1 = 1
    f n = fib (n - 2) + fib (n - 1)

fib' n = (fibs !! n) where
    fibs = [ f x | x <- [0 .. ] ]
    f 0 = 0
    f 1 = 1
    f n = fibs !! (n - 2) + fibs !! (n - 1)

solutions' = [ ((c, n), ways c n) | n <- [0 .. ], c <- [1 .. length us] ] where
    ways 0 _ = 0
    ways _ 0 = 1
    ways c n | n < 0 = 0
             | otherwise = ways c (n - (us !! (c - 1))) + ways (c - 1) n

mways coins n = solutions !! idx n (length coins) where
    solutions = [ twoWays c n | n <- [0 .. ], c <- [1 .. length coins] ]
    twoWays c n = ways c (n - (coins !! (c - 1))) + ways (c - 1) n
    ways 0 _ = 0
    ways _ 0 = 1
    ways c n = if n < 0 then 0 else solutions !! idx n c
    idx n c = (n * length coins) + (c - 1)

-- Memoizing version. On the way down we build up a map of every
-- value we compute so we don't compute them over and over again.

emptyTable :: M.Map ([Int], Int) Integer
emptyTable = M.empty

ways' coins n = fst $ f coins n emptyTable where
    f [] _ m     = (0, m)
    f _ 0 m      = (1, m)
    f (c:cs) n m = if n < 0 then (0, m) else (a + a', m'') where
        (a, m')   = recur (c:cs) (n - c) m
        (a', m'') = recur cs n m'
    recur cs n m = maybe compute wrap (M.lookup (cs, n) m) where
        compute  = let (a, m') = f cs n m in (a, M.insert (cs, n) a m')
        wrap a   = (a, m)


-- Memoizing using the State monad. Similar to above except that the
-- Map of memoized values is threaded through the monad rather than
-- explicitly which makes things a bit more tidy.

ways'' coins n = evalState (f coins n) emptyTable where
    f [] _     = return 0
    f _ 0      = return 1
    f (c:cs) n = if n < 0 then return 0 else (+) <$> recur (c:cs) (n - c) <*> recur cs n
    recur c n = get >>= maybe (compute c n) return . M.lookup (c, n)
    compute c n = mapState (memoize (c, n)) $ f c n
    memoize k (x, m) = (x, M.insert k x m)


-- Problem with that style of memoizing is that the table we build up
-- of previously computed values eventually contains every value we've
-- ever needed. Turns out that's silly because if we work from the
-- bottom up we can quickly discard many of the early answers as
-- they're no longer needed.

-- Our first approach here is to hand roll some linked together
-- infinite lists. The nth item in these lists represents the number
-- of ways to make change for n+1 cents with the given kind of coin
-- and all the preceeding ones.

pennies     = repeat 1
nickles     = zipWith (+) pennies (replicate 4 0 ++ [1] ++ nickles)
dimes       = zipWith (+) nickles (replicate 9 0 ++ [1] ++ dimes)
quarters    = zipWith (+) dimes (replicate 24 0 ++ [1] ++ quarters)
halfDollars = zipWith (+) quarters (replicate 49 0 ++ [1] ++ halfDollars)

-- Now we can abstract the pattern of making those lists into a
-- function of the list for the previous coin and the denomination of
-- the current coin.

coin prev n = this where this = zipWith (+) prev (replicate (n - 1) 0 ++ [1] ++ this)

-- Use that function to make a stack of infinite lists that should be
-- equivalent to the hand-rolled ones before. Just for consistency,
-- this time we'll use the function to create the pennies list too by
-- using a previous coin of 0.

pennies'     = coin (repeat 0) 1
nickles'     = coin pennies' 5
dimes'       = coin nickles' 10
quarters'    = coin dimes' 25
halfDollars' = coin quarters' 50

-- Now it's pretty obvious that we can produce the final list for that
-- set of denominations, or any other, with a fold using the coin
-- function. If we tack a 1 on the front so the nth element of the
-- list is the number of ways to make change for n cents, we can
-- easily write a function that regenerates the list each time so the
-- front of the list can get GC'd.

ways''' coins = ((1 : foldl coin (repeat 0) coins) !!)

benchmarks = [ ("naive", ways)
             , ("lazy one", mways)
             , ("memoizing", ways')
             , ("memoizing w/State", ways'')
             , ("lazy two", ways''')
             ]

time :: ([Int] -> Int -> Integer) -> Int -> IO (Double, Integer)
time fn n = do
  start <- getCPUTime
  x     <- evaluate $ force $ fn us n
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  return (diff, x)

emit :: String -> Int -> (Double, Integer) -> IO ()
emit label n (time, result) = printf "%s %d: %0.9f sec. Result %d\n" label n time result

check label fn n = do
  (time, r) <- time fn n
  emit label n (time, r)
  if time < 2 then check label fn (n * 10) else return ()

--main =  mapM_ (\(label, fn, n) -> (time (fn :: ([Int] -> Int -> Integer)) n) >>= (emit label n)) $ [ (label, fn, n)  | (label, fn) <- benchmarks, n <- [100, 1000, 10000, 100000, 1000000]]

main = mapM_ (\(label, fn) -> check label fn 1) benchmarks

--main = print $ (tWays us 100)
