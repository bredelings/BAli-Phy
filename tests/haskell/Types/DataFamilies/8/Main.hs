{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (putStrLn)

data A = A
data B = B

data family F a
data family G a

data instance F A = MkF { getF :: A }

data instance G a where
    MkG :: a -> G a

regular :: F A -> A
regular x = getF x

gadt :: G B -> B
gadt (MkG x) = x

main = do
    case regular (MkF A) of
        A -> putStrLn "regular data instance ok"
    case gadt (MkG B) of
        B -> putStrLn "gadt data instance ok"
