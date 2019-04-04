module AddThree where

foreign import ccall "c_add_one" addOne :: Int -> Int
foreign import ccall "c_add_two" addTwo :: Int -> Int

addThree :: Int -> Int
addThree = \x -> addOne x + addTwo x
