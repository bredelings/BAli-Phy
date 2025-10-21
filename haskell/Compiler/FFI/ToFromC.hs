{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.FFI.ToFromC where

import Compiler.Base (String)
import Compiler.IO
import Foreign.Vector
import Foreign.Pair
import Foreign.String
import Data.Bool    
import Data.Function
import Data.List (map)
import Numeric.LogDouble (LogDouble)

class ToFromC a where
    type ToC a
    toC :: a -> ToC a
    fromC :: ToC a -> a

instance ToFromC () where
    type ToC () = ()
    toC = id
    fromC = id

instance ToFromC Int where
    type ToC Int = Int
    toC = id
    fromC = id

instance ToFromC Char where
    type ToC Char = Char
    toC = id
    fromC = id

instance ToFromC Double where
    type ToC Double = Double
    toC = id
    fromC = id

instance ToFromC LogDouble where
    type ToC LogDouble = LogDouble
    toC = id
    fromC = id

instance ToFromC Bool where
    type ToC Bool = Bool
    toC = id
    fromC = id

instance (ToFromC a, ToFromC b) => ToFromC (a->b) where
    type ToC (a->b) = ToC a -> ToC b

    fromC f = fromC . f . toC
    toC f = toC . f . fromC

instance ToFromC (IO a) where
    type ToC (IO a) = RealWorld -> a
    toC i = (\s -> let (_,x) = runIO i s in x)
    fromC i = makeIO i

instance ToFromC a => ToFromC [a] where
    type ToC [a] = EVector (ToC a)

    toC xs = listToVector $ map toC xs
    fromC v = map fromC $ vectorToList v

instance (ToFromC a, ToFromC b) => ToFromC (a, b) where
    type ToC (a, b) = EPair (ToC a) (ToC b)

    toC (x,y) = c_pair (toC x) (toC y)
    fromC xy = (fromC $ c_fst xy, fromC $ c_snd xy)

instance  {-# INCOHERENT #-} ToFromC String where
    type ToC String = CPPString

    toC = pack_cpp_string
    fromC = unpack_cpp_string
