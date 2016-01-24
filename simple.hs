-- Relatively simple recursive solution: the number of ways to make
-- change with a given set of coins is all the ways you can make
-- change with at least one of the first coin plus all the ways you
-- can make change with none of the first coin. The first of those is
-- same as the number of ways you can make change for the original
-- amount minus the denomination of the first coin with all the coins.
-- Note that this function is very slow. It's fast enough to compute
-- the ways to compute change for a dollar using the standard us coins
-- up to half-dollar but for larger amounts it quickly bogs down.

module Simple (ways) where

ways :: [Int] -> Int -> Integer
ways [] _     = 0
ways _ 0      = 1
ways (c:cs) n = if n < 0 then 0 else ways (c:cs) (n - c) + ways cs n
