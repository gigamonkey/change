import Control.Monad.State
import qualified Data.Map.Strict as M

us :: Num a => [a]
us = [1,5,10,25,50]

-- Relatively simple recursive solution: the number of ways to make
-- change with a given set of coins is all the ways you can make
-- change with at least one of the first coin plus all the ways you
-- can make change with none of the first coin. The first of those is
-- same as the number of ways you can make change for the original
-- amount minus the denomination of the first coin with all the coins.

ways [] _ = 0
ways _ 0  = 1
ways (c:cs) n | n < 0 = 0
              | otherwise = ways (c:cs) (n - c) + ways cs n

-- Memoizing version. On the way down we build up a map of every
-- value we compute so we don't compute them over and over again.

mways coins n = fst $ f coins n M.empty where
    f [] _ m     = (0, m)
    f _ 0 m      = (1, m)
    f (c:cs) n m | n < 0 = (0, m)
                 | otherwise = (a + a', m'') where
                   (a, m')   = foo (c:cs) (n - c) m
                   (a', m'') = foo cs n m'
    foo cs n m = maybe compute wrap memoized where
        memoized = M.lookup (cs, n) m
        compute  = let (a, m') = f cs n m in (a, M.insert (cs, n) a m')
        wrap a   = (a, m)

type Memotable = M.Map ([Int], Int) Int

sways coins n = fst $ runState (f coins n) M.empty where
    f [] _ = return 0 :: State Memotable Int
    f _ 0  = return 1
    f (c:cs) n = if n < 0 then return 0 else (+) <$> memo (c:cs) (n - c) <*> memo cs n
    memo c n = do
      m <- get
      x <- maybe (f c n) return (M.lookup (c, n) m)
      modify $ M.insert (c, n) x
      return x

-- Hand roll some linked together infinite lists. Note however, that
-- the first item of these lists represents the number of ways to make
-- change for 1 cent.

pennies     = repeat 1
nickles     = zipWith (+) pennies (replicate 4 0 ++ [1] ++ nickles)
dimes       = zipWith (+) nickles (replicate 9 0 ++ [1] ++ dimes)
quarters    = zipWith (+) dimes (replicate 24 0 ++ [1] ++ quarters)
halfDollars = zipWith (+) quarters (replicate 49 0 ++ [1] ++ halfDollars)

-- Now abstract the pattern into a function
coin :: [Integer] -> Integer -> [Integer]
coin prev n = this where this = zipWith (+) prev (replicate (fromInteger n - 1) 0 ++ [1] ++ this)

-- Use that function to make a stack of infinite lists that should be
-- equivalent to the hand-rolled ones before. Just for consistency we
-- use coin to create the pennies list too.
pennies'     = coin (repeat 0) 1
nickles'     = coin pennies' 5
dimes'       = coin nickles' 10
quarters'    = coin dimes' 25
halfDollars' = coin quarters' 50

-- Now generalize into a function that given a list of denominations
-- folds together the set of infinite lists. And we tack a 1 on the
-- front so the nth element of the list is the number of ways to make
-- change for n cents. Written as a function like this it regenerates
-- the list each time but that means the front of the list can get GC'd.
ways' coins = ((1 : foldl coin (repeat 0) coins) !!)

main = do
  print $ ways us 100
  print $ mways us 10000
  print $ sways us 10000
  print $ (1:halfDollars) !! 100000
  print $ (1:halfDollars') !! 100000
  print $ ways' us 100000
