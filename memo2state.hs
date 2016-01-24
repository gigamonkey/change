module Memo2State (ways) where

import Control.Monad.State
import qualified Data.Map.Strict as M

-- Memoizing using the State monad. Similar to above except that the
-- Map of memoized values is threaded through the monad rather than
-- explicitly which makes things a bit more tidy.

ways :: [Int] -> Int -> Integer
ways coins amount = evalState (f coins amount) M.empty where
    f [] _     = return 0
    f _ 0      = return 1
    f (c:cs) n = if n < 0 then return 0 else (+) <$> recur (c:cs) (n - c) <*> recur cs n
    recur c n = get >>= maybe (compute c n) return . M.lookup (c, n)
    compute c n = mapState (memoize (c, n)) $ f c n
    memoize k (x, m) = (x, M.insert k x m)
