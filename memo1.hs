module Memo1 (ways) where

ways :: [Int] -> Int -> Integer
ways coins amount = solutions !! idx amount (length coins) where
    solutions = [ twoWays c n | n <- [0 .. ], c <- [1 .. length coins] ]
    twoWays c n = w c (n - (coins !! (c - 1))) + w (c - 1) n
    w 0 _ = 0
    w _ 0 = 1
    w c n = if n < 0 then 0 else solutions !! idx n c
    idx n c = (n * length coins) + (c - 1)
