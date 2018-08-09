module MyModule where

foreign import ccall get_thing :: IO Int

getThing :: IO Int
getThing = get_thing

foreign import ccall set_thing :: Int -> IO ()

setThing :: Int -> IO ()
setThing = set_thing
