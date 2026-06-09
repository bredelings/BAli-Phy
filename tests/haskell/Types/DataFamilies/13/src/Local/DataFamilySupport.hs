{-# LANGUAGE NoImplicitPrelude #-}

module Local.DataFamilySupport where

import Control.Monad (return)

data A = A
data B = B

data family F a

data instance F A = FA ()
data instance F B = FB1 | FB2

makeFA = return (FA ())

message = "hierarchical data family bind ok"
