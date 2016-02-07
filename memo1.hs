module Memo1 (ways) where

-- Similar to fib functions, with a bit of cleverness we can generate
-- an infinite list of solutions to the change problem for a given
-- (finite) set of coins by listing for each amount, n, the number of
-- ways to make change with the first c coins. The recursive call to
-- the function used to generate this list can index into the list to
-- get already computed values.

ways :: [Int] -> Int -> Integer
ways coins amount = solutions !! idx amount (length coins)
    where
      solutions = [ twoWays c n | n <- [0 .. ], c <- [1 .. length coins] ]
      twoWays c n = ways' c (n - (coins !! (c - 1))) + ways' (c - 1) n
      ways' 0 _ = 0
      ways' _ 0 = 1
      ways' c n = if n < 0 then 0 else solutions !! idx n c
      idx n c = (n * length coins) + (c - 1)
