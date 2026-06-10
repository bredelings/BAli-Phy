{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

class Twice a where
  twice :: a -> a

instance Twice Int where
  twice x = x + x

newtype Age = Age Int deriving newtype Twice

class MapLike f where
  mapLike :: (a -> b) -> f a -> f b

data Box a = Box a

instance MapLike Box where
  mapLike f (Box x) = Box (f x)

newtype NewBox a = NewBox (Box a) deriving newtype MapLike

class Offset tag a where
  offset :: tag -> a -> Int

instance Offset Int Int where
  offset tag x = tag + x

newtype Count = Count Int deriving newtype (Offset Int)

class Sized a where
  type SizeArg a
  sizeArg :: a -> SizeArg a

instance Sized Int where
  type SizeArg Int = Int
  sizeArg x = x

newtype Size = Size Int deriving newtype Sized

class Tagged tag a where
  type TaggedArg tag a
  taggedArg :: tag -> a -> TaggedArg tag a

instance Tagged Int Int where
  type TaggedArg Int Int = Int
  taggedArg tag x = tag + x

newtype TaggedCount = TaggedCount Int deriving newtype (Tagged Int)

data StockColor = StockRed | StockBlue deriving stock Eq

class Needs b where
  need :: b -> Int

instance Needs Int where
  need x = x

class Higher a where
  higher :: forall b. Needs b => a -> b -> a

instance Higher Int where
  higher x y = x + need y

newtype HigherAge = HigherAge Int deriving newtype Higher

unAge (Age x) = x
unNewBox (NewBox (Box x)) = x
unHigherAge (HigherAge x) = x

ok = unAge (twice (Age 21)) == 42
  && unNewBox (mapLike (\x -> x + 1) (NewBox (Box 41))) == 42
  && offset (1 :: Int) (Count 41) == 42
  && sizeArg (Size 42) == 42
  && taggedArg (1 :: Int) (TaggedCount 41) == 42
  && StockRed == StockRed
  && unHigherAge (higher (HigherAge 40) (2 :: Int)) == 42

main = print (if ok then (1 :: Int) else 0)
