{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Bool where

data TrueType
data FalseType

type family If cond tru fls where
    If TrueType  tru fls = tru
    If FalseType tru fls = fls

type family AndType a b where
    AndType FalseType b         = FalseType
    AndType TrueType  b         = TrueType
    AndType a         FalseType = FalseType
    AndType a         TrueType  = TrueType
    AndType a         a         = a
-- infixr 2 &&

type family OrType a b where
  OrType FalseType a         = a
  OrType TrueType  a         = TrueType
  OrType a         FalseType = a
  OrType a         TrueType  = TrueType
  OrType a         a         = a
-- infixr 2 ||

type family Not a where -- Not a = res | res -> a
    Not FalseType = TrueType
    Not TrueType  = FalseType
