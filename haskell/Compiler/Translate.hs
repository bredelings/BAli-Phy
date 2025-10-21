{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Translate where

import Compiler.Base (String)
import Compiler.IO
import Foreign.Vector
import Foreign.Pair
import Foreign.String
import Data.Bool    
import Data.Function
import Data.List (map)
import Numeric.LogDouble (LogDouble)

class Translate a where
    type ToC a
    toC :: a -> ToC a
    fromC :: ToC a -> a

instance Translate () where
    type ToC () = ()
    toC = id
    fromC = id

instance Translate Int where
    type ToC Int = Int
    toC = id
    fromC = id

instance Translate Char where
    type ToC Char = Char
    toC = id
    fromC = id

instance Translate Double where
    type ToC Double = Double
    toC = id
    fromC = id

instance Translate LogDouble where
    type ToC LogDouble = LogDouble
    toC = id
    fromC = id

instance Translate Bool where
    type ToC Bool = Bool
    toC = id
    fromC = id

instance (Translate a, Translate b) => Translate (a->b) where
    type ToC (a->b) = ToC a -> ToC b

    fromC f = fromC . f . toC
    toC f = toC . f . fromC

instance Translate (IO a) where
    type ToC (IO a) = RealWorld -> a
    toC i = (\s -> let (_,x) = runIO i s in x)
    fromC i = makeIO i

instance Translate a => Translate [a] where
    type ToC [a] = EVector (ToC a)

    toC xs = listToVector $ map toC xs
    fromC v = map fromC $ vectorToList v

instance (Translate a, Translate b) => Translate (a, b) where
    type ToC (a, b) = EPair (ToC a) (ToC b)

    toC (x,y) = c_pair (toC x) (toC y)
    fromC xy = (fromC $ c_fst xy, fromC $ c_snd xy)

instance  {-# INCOHERENT #-} Translate String where
    type ToC String = CPPString

    toC = pack_cpp_string
    fromC = unpack_cpp_string
