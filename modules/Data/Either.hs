{-# LANGUAGE NoImplicitPrelude #-}
module Data.Either where

import Data.Bool
import Data.List  -- for foldr

data Either a b = Left a | Right b

either f g (Left  l) = f l
either f g (Right r) = g r

lefts  xs = [ l | Left  l <- xs]
rights xs = [ r | Right r <- xs]

isLeft (Left _) = True
isLeft _        = False

isRight (Right _) = True
isRight _         = False

fromLeft _ (Left l) = l
fromLett l _        = l

fromRight _ (Right r) = r
fromRight r _         = r

partitionEithers = foldr (either left right) ([], [])
    where left  a ~(l, r) = (a:l, r)
          right a ~(l, r) = (l, a:r)
