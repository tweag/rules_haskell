module TogetherTest (main) where

import One (one)
import Two (two)

main :: IO ()
main = putStrLn $ "One and Two makes " ++ show (one + two)
