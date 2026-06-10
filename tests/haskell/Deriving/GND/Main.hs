{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

class Twice a where
  twice :: a -> a

instance Twice Int where
  twice x = x + x

newtype Age = Age Int deriving Twice

class MapLike f where
  mapLike :: (a -> b) -> f a -> f b

data Box a = Box a

instance MapLike Box where
  mapLike f (Box x) = Box (f x)

newtype NewBox a = NewBox (Box a) deriving MapLike

class Offset tag a where
  offset :: tag -> a -> Int

instance Offset Int Int where
  offset tag x = tag + x

newtype Count = Count Int deriving (Offset Int)

class Sized a where
  type SizeArg a
  sizeArg :: a -> SizeArg a

instance Sized Int where
  type SizeArg Int = Int
  sizeArg x = x

newtype Size = Size Int deriving Sized

class Tagged tag a where
  type TaggedArg tag a
  taggedArg :: tag -> a -> TaggedArg tag a

instance Tagged Int Int where
  type TaggedArg Int Int = Int
  taggedArg tag x = tag + x

newtype TaggedCount = TaggedCount Int deriving (Tagged Int)

unAge (Age x) = x
unNewBox (NewBox (Box x)) = x

ok = unAge (twice (Age 21)) == 42
  && unNewBox (mapLike (\x -> x + 1) (NewBox (Box 41))) == 42
  && offset (1 :: Int) (Count 41) == 42
  && sizeArg (Size 42) == 42
  && taggedArg (1 :: Int) (TaggedCount 41) == 42

main = print (if ok then (1 :: Int) else 0)
