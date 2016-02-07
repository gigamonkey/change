module Memo2 (ways) where

import qualified Data.Map.Strict as M

-- Memoizing version. On the way down we build up a map of every value
-- we compute so we don't compute them over and over again. In this
-- version we explicitly pass a map along with the list of coins

ways :: [Int] -> Int -> Integer
ways coins amount = fst $ f coins amount M.empty where
    f [] _ m     = (0, m)
    f _ 0 m      = (1, m)
    f (c:cs) n m = if n < 0 then (0, m) else (a + a', m'')
        where
          (a, m')   = memo (c:cs) (n - c) m
          (a', m'') = memo cs n m'
    memo cs n m = maybe compute wrap (M.lookup (cs, n) m)
        where
          compute  = let (a, m') = f cs n m in (a, M.insert (cs, n) a m')
          wrap a   = (a, m)
