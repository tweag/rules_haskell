module ModuleWith (part1) where

part1 :: String
part1 =
  if "plugin" == ['a', 'c', 't', 'i', 'v', 'e'] then
    "hello,"
  else
    error "plugin not loaded"
