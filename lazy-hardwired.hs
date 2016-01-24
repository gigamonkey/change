-- Problem with that style of memoizing is that the table we build up
-- of previously computed values eventually contains every value we've
-- ever needed. Turns out that's silly because if we work from the
-- bottom up we can quickly discard many of the early answers as
-- they're no longer needed.

-- Our first approach here is to hand roll some linked together
-- infinite lists. The nth item in these lists represents the number
-- of ways to make change for n+1 cents with the given kind of coin
-- and all the preceeding ones.

pennies, nickles, dimes, quarters, halfDollars :: [Integer]

pennies     = repeat 1
nickles     = zipWith (+) pennies (replicate 4 0 ++ [1] ++ nickles)
dimes       = zipWith (+) nickles (replicate 9 0 ++ [1] ++ dimes)
quarters    = zipWith (+) dimes (replicate 24 0 ++ [1] ++ quarters)
halfDollars = zipWith (+) quarters (replicate 49 0 ++ [1] ++ halfDollars)

ways :: Int -> Integer
ways = ((1 : halfDollars) !!)

main :: IO ()
main = print $ ways 10000
