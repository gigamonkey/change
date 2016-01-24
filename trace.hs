import Debug.Trace

us :: Num a => [a]
us = [1,5,10,25,50]

ways :: [Int] -> Int -> Integer
ways cs@[]  n   = trace (show (cs, n)) 0
ways cs     n@0 = trace (show (cs, n)) 1
ways (c:cs) n   = trace (show (c:cs, n)) (if n < 0 then 0 else ways (c:cs) (n - c) + ways cs n)

main :: IO ()
main = print $ ways us 100
