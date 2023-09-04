module Main where

main =
  if "plugin" == ['p', 'l', 'u', 'g', 'i', 'n', '2'] then
    error "unexpected plugin2 loaded"
  else
    putStrLn "hello world"
