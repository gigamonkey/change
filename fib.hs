fib, fib' :: Int -> Integer

fib = (map f [0 .. ] !!) where
    f 0 = 0
    f 1 = 1
    f n = fib (n - 2) + fib (n - 1)

fib' n = fibs !! n where
    fibs = [ f x | x <- [0 .. ] ]
    f 0 = 0
    f 1 = 1
    f n' = fibs !! (n' - 2) + fibs !! (n' - 1)

main :: IO ()
main = do
  print $ fib 100
  print $ fib' 100
