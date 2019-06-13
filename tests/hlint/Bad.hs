module Bad where

bad :: [Int]
bad = concat $ map (\n -> [n, n + 1]) $ [1, 3, 5]
