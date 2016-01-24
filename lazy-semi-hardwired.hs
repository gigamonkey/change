-- Now we can abstract the pattern of making those lists into a
-- function of the list for the previous coin and the denomination of
-- the current coin.

coin :: [Integer] -> Int -> [Integer]
coin prev n = this where this = zipWith (+) prev (replicate (n - 1) 0 ++ [1] ++ this)

-- Use that function to make a stack of infinite lists that should be
-- equivalent to the hand-rolled ones before. Just for consistency,
-- this time we'll use the function to create the pennies list too by
-- using a previous coin of 0.

pennies, nickles, dimes, quarters, halfDollars :: [Integer]

pennies     = coin (repeat 0) 1
nickles     = coin pennies 5
dimes       = coin nickles 10
quarters    = coin dimes 25
halfDollars = coin quarters 50

ways :: Int -> Integer
ways = ((1 : halfDollars) !!)

main :: IO ()
main = print $ ways 1000
