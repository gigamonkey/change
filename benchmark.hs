import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.CPUTime
import Text.Printf
import qualified Simple
import qualified Memo1
import qualified Memo2
import qualified Memo2State
import qualified Lazy

us :: [Int]
us = [1,5,10,25,50]

benchmarks :: [(String, [Int] -> Int -> Integer)]
benchmarks = [ ("simple", Simple.ways)
             , ("memo one", Memo1.ways)
             , ("memo two", Memo2.ways)
             , ("memo two  w/State", Memo2State.ways)
             , ("lazy", Lazy.ways)
             ]

time :: ([Int] -> Int -> Integer) -> Int -> IO (Double, Integer)
time fn n = do
  start <- getCPUTime
  x     <- evaluate $ force $ fn us n
  end   <- getCPUTime
  return (fromIntegral (end - start) / (10 ** 12), x)

emit :: String -> Int -> (Double, Integer) -> IO ()
emit label n (t, r) = printf "%s %d: %0.9f sec. Result %d\n" label n t r

check :: String -> ([Int] -> Int -> Integer) -> Int -> IO ()
check label fn n = do
  (t, r) <- time fn n
  emit label n (t, r)
  when (t < 1) $ check label fn (n * 10)

main :: IO ()
main = mapM_ (\(label, fn) -> check label fn 1) benchmarks
