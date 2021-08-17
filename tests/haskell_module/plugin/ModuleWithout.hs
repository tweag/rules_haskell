module ModuleWithout (part2) where

part2 :: String
part2 =
  if "plugin" == ['a', 'c', 't', 'i', 'v', 'e'] then
    error "plugin loaded unexpectedly"
  else
    " world"
