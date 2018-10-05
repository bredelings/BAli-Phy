{-# LANGUAGE NoImplicitPrelude #-}
module Data.Maybe where

import Data.Bool

data Maybe a = Just a | Nothing

maybe n _ Nothing  = n
maybe _ f (Just x) = f x

isJust Nothing = False
isJust _       = True

isNothing Nothing = True
isNothing _       = False

fromJust (Just x) = x
-- fromJust Nothing  = error "Maybe.fromJust: Nothing"

fromMaybe d x = case x of Nothing -> d
                          Just v -> v

maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe [] = Nothing
listToMaybe (x:_) = Just x
-- listToMaybe = foldr (const . Just) Nothing
-- GHC uses this to fused via the foldr/build rule.

-- catMaybes ls = [ x | Just x <- ls]

mapMaybes _     [] = []
mapMaybes f (x:xs) = let rs = mapMaybes f xs
                     in case f x of Nothing -> rs
                                    Just r  -> r:rs
