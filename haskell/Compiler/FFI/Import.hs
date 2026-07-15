{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.FFI.Import
    ( CInput(..)
    , COutput(..)
    , RawImport
    , fromCImport
    ) where

import Compiler.Base (String)
import Compiler.IO (IO, RealWorld, makeIO)
import Data.Bool (Bool)
import Data.Function (id)
import Data.Functor ((<$>))
import Foreign.Pair (EPair, c_fst, c_snd)
import Foreign.String (CPPString, pack_cpp_string, unpack_cpp_string)
import Numeric.LogDouble (LogDouble)

-- | Translate one Haskell argument into the raw argument sequence consumed by
-- an imported builtin.  The continuation makes it possible for one public
-- argument to occupy any number of raw slots.
class CInput a where
    type CInputType a result
    type CInputType a result = a -> result

    withCInput :: a -> CInputType a result -> result
    default withCInput
        :: CInputType a result ~ (a -> result)
        => a -> CInputType a result -> result
    withCInput value continuation = continuation value

-- | Translate the single raw result of an imported builtin into its Haskell
-- result.  Input and output capabilities are intentionally independent.
class COutput a where
    type COutputType a
    type COutputType a = a

    fromCOutput :: COutputType a -> a
    default fromCOutput :: COutputType a ~ a => COutputType a -> a
    fromCOutput = id

-- | Compute the complete raw signature of an imported builtin from its public
-- Haskell signature.  The order of these closed equations is significant: an
-- unknown type cannot use the result equation until it is known not to be a
-- function.
type family RawImport f where
    RawImport (input -> rest) = CInputType input (RawImport rest)
    RawImport result = COutputType result

class BuildImport f where
    buildImport :: RawImport f -> f

instance {-# OVERLAPPING #-}
         (CInput input, BuildImport rest) =>
         BuildImport (input -> rest) where
    buildImport raw input = buildImport (withCInput input raw)

instance {-# OVERLAPPABLE #-}
         (COutput result, RawImport result ~ COutputType result) =>
         BuildImport result where
    buildImport = fromCOutput

fromCImport :: BuildImport f => RawImport f -> f
fromCImport = buildImport

instance CInput ()
instance COutput ()

instance CInput Int
instance COutput Int

instance CInput Char
instance COutput Char

instance CInput Double
instance COutput Double

instance CInput Bool
instance COutput Bool

instance CInput LogDouble
instance COutput LogDouble

instance CInput String where
    type CInputType String result = CPPString -> result
    withCInput value continuation = continuation (pack_cpp_string value)

instance COutput String where
    type COutputType String = CPPString
    fromCOutput = unpack_cpp_string

instance (CInput a, CInput b) => CInput (a,b) where
    type CInputType (a,b) result = CInputType a (CInputType b result)
    withCInput (x,y) continuation =
        withCInput y (withCInput x continuation)

instance (COutput a, COutput b) => COutput (a,b) where
    type COutputType (a,b) = EPair (COutputType a) (COutputType b)
    fromCOutput pair =
        (fromCOutput (c_fst pair), fromCOutput (c_snd pair))

-- EPair is already an opaque runtime value.  Unlike a Haskell tuple, passing
-- or returning it does not translate its two elements into separate slots.
instance CInput (EPair a b)
instance COutput (EPair a b)

instance COutput a => COutput (IO a) where
    type COutputType (IO a) = RealWorld -> COutputType a
    fromCOutput raw = fromCOutput <$> makeIO raw
