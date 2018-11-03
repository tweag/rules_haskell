import           Criterion.Main
import           Lib

main :: IO ()
main = defaultMain
  [ bench "AddOne" $ whnf addOne 5
  ]
