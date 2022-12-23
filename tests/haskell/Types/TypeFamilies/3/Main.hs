{-# LANGUAGE NoImplicitPrelude #-}

import Data.Maybe

class C a where
    method :: a -> a

instance C (Maybe a) where
    method x = x

type family F a

func :: a ~ Maybe (F a) => a -> a
func x = method x
