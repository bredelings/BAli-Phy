{-# LANGUAGE NoImplicitPrelude #-}
-- Adapted from GHC (1543f834476ca41338df87e74dfa3477279486a9).
-- Copyright (c) Ashley Yakeley 2007. See LICENSE.GHC for the upstream license.
-- Only the ordinary function category is provided here; coercion and type-equality
-- instances require types not currently supplied by this library.
module Control.Category (Category(..), (>>>), (<<<)) where

import qualified Data.Function as Function

infixr 9 .
infixr 1 >>>, <<<

-- Identity and composition for categories, including ordinary functions.
class Category cat where
    -- | The identity morphism. Implementations should satisfy two laws:
    --
    -- [Right identity] @f '.' 'id'  =  f@
    -- [Left identity]  @'id' '.' f  =  f@
    --
    -- These essentially state that 'id' should "do nothing".
    id :: cat a a

    -- | Morphism composition. Implementations should satisfy the law:
    --
    -- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
    --
    -- This means that the way morphisms are grouped is irrelevant, so it is unambiguous
    -- to write a composition of morphisms as @f '.' g '.' h@, without parentheses.
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = Function.id
    (.) = (Function..)

-- Right-to-left composition.
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- Left-to-right composition.
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f
