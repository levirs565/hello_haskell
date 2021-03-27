module Lib
    ( someFunc
    ) where

helloFunction :: String -> IO ()
helloFunction = putStrLn

someFunc :: IO ()
someFunc = putStrLn "someFunc"
