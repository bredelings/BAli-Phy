{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (return)
import System.IO (IO, putStrLn)

data A = A
data B = B

data family F a

data instance F A = FA ()
data instance F B = FB1 | FB2

main :: IO ()
main = do
    FA () <- return (FA ())
    putStrLn "single-constructor data family bind ok"
