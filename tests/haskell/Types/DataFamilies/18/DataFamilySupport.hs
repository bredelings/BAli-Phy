{-# LANGUAGE NoImplicitPrelude #-}

module DataFamilySupport
    ( A(..)
    , F(..)
    , makeFA
    , message
    ) where

import Control.Monad (return)

data A = A
data B = B

data family F a

data instance F A = FA ()
data instance F B = FB1 | FB2

makeFA = return (FA ())

message = "data family export list ok"
