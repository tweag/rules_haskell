{-# LANGUAGE TemplateHaskell #-}

module Lib where
import SubLib

libVal = subLibVal ++ " through Lib " ++ show $([| ten |])
