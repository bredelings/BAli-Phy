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
    withCInput :: a -> CInputType a result -> result

-- | Translate the single raw result of an imported builtin into its Haskell
-- result.  Input and output capabilities are intentionally independent.
class COutput a where
    type COutputType a
    fromCOutput :: COutputType a -> a

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

instance CInput () where
    type CInputType () result = () -> result
    withCInput value continuation = continuation value

instance COutput () where
    type COutputType () = ()
    fromCOutput = id

instance CInput Int where
    type CInputType Int result = Int -> result
    withCInput value continuation = continuation value

instance COutput Int where
    type COutputType Int = Int
    fromCOutput = id

instance CInput Char where
    type CInputType Char result = Char -> result
    withCInput value continuation = continuation value

instance COutput Char where
    type COutputType Char = Char
    fromCOutput = id

instance CInput Double where
    type CInputType Double result = Double -> result
    withCInput value continuation = continuation value

instance COutput Double where
    type COutputType Double = Double
    fromCOutput = id

instance CInput Bool where
    type CInputType Bool result = Bool -> result
    withCInput value continuation = continuation value

instance COutput Bool where
    type COutputType Bool = Bool
    fromCOutput = id

instance CInput LogDouble where
    type CInputType LogDouble result = LogDouble -> result
    withCInput value continuation = continuation value

instance COutput LogDouble where
    type COutputType LogDouble = LogDouble
    fromCOutput = id

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

instance COutput a => COutput (IO a) where
    type COutputType (IO a) = RealWorld -> COutputType a
    fromCOutput raw = fromCOutput <$> makeIO raw
