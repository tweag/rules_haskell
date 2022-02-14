{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "_Z10print_thatRNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE" print_that :: Int -> IO ()

main :: IO ()
main = print_that 0
