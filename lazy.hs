-- Now it's pretty obvious that we can produce the final list for that
-- set of denominations, or any other, with a fold using the coin
-- function. If we tack a 1 on the front so the nth element of the
-- list is the number of ways to make change for n cents, we can
-- easily write a function that regenerates the list each time so the
-- front of the list can get GC'd.

module Lazy (ways) where

coin :: [Integer] -> Int -> [Integer]
coin prev n = this where this = zipWith (+) prev (replicate (n - 1) 0 ++ [1] ++ this)

ways :: [Int] -> Int -> Integer
ways coins = ((1 : foldl coin (repeat 0) coins) !!)
