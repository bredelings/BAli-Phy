{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (IO, putStrLn)

data A = A
data B = B

data family F a

data instance F A = FA
data instance F B = FB

main :: IO ()
main = case FA of
    FA -> putStrLn "surely apart data instances ok"
