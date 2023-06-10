{-# LANGUAGE NoImplicitPrelude #-}
module Control.Applicative where

import Data.Functor
import Data.Function
import Data.Maybe

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b 
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a

    (<*>) = liftA2 id
    liftA2 f x y = f <$> x <*> y
    as *> bs = (\x y -> y) <$> as <*> bs
    as <* bs = (\x y -> x) <$> as <*> bs


infixl 3 <|>

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    some :: f a -> f [a]
    many :: f a -> f [a]

    many v = some v <|> pure []
--  Not working! some v = (:) <$> v <*> many v

-- These are defined by Parse.hs
-- <|>
-- some
-- many

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative Maybe where
    pure x = Just x

    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing
