module Main where

main =
  if "plugin" == ['p', 'l', 'u', 'g', 'i', 'n', '1'] then
    putStrLn "hello world"
  else
    error "plugin1 not loaded"
