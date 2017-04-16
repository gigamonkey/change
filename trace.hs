import Debug.Trace

us :: Num a => [a]
us = [1,5,10,25,50]

ways :: [Int] -> Int -> Integer
ways cs@[] n      = trace ("ways " ++ show cs ++ " " ++ show n ++ " => 0") 0
ways cs n@0       = trace ("ways " ++ show cs ++ " " ++ show n ++ " => 1") 1
ways cs n | n < 0 = trace ("ways " ++ show cs ++ " " ++ show n ++ " => 0") 0
ways (c:cs) n     = trace ("ways " ++ show (c:cs) ++ " " ++ show n ++ " => ways " ++ show cs ++ " " ++ show n ++ " + " ++ "ways " ++ show (c:cs) ++ " " ++ show (n - c)) (ways cs n + ways (c:cs) (n - c))

main :: IO ()
main = print $ ways us 100
