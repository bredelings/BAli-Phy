{-# LANGUAGE NoImplicitPrelude #-}
module Control.Applicative where

import Data.Functor

class Functor f => Applicative f

f <*> x = do fresult <- f
             xresult <- x
             pure (fresult xresult)

liftA2 f as bs = do a <- as
                    b <- bs
                    pure (f a b)

as *> bs = do as
              b <- bs
              pure b

as <* bs = do a <- as
              bs
              pure a

-- empty

-- These are defined by Parse.hs
-- <|>
-- some
-- many

