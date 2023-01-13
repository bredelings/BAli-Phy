{-# LANGUAGE NoImplicitPrelude #-}
module Data.List
    (
      module Data.Foldable
--    , module Data.Traversable
    , module Data.OldList
    ) where

import Data.Foldable hiding (toList)
--import Data.Traversable
import Data.OldList hiding (all, any, and, concat, concatMap, elem, find,
                            foldl, foldl1, foldl', foldr, foldr1,
--                            mapAccumL, mapAccumR, -- from Traversable,
                            fold, foldMap, foldMap', fold, foldr, foldr', foldl, foldl',
                            maximum, minimum, -- maximumBy, minimumBy,
                            length, notElem, null, or, product, sum)

