module Main where

main =
  if "plugin" == ['p', 'l', 'u', 'g', 'i', 'n', '2'] then
    putStrLn "hello world"
  else
    error "plugin2 not loaded"
