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
    type Tr a
    toC :: a -> Tr a
    fromC :: Tr a -> a

instance Translate () where
    type Tr () = ()
    toC = id
    fromC = id

instance Translate Int where
    type Tr Int = Int
    toC = id
    fromC = id

instance Translate Char where
    type Tr Char = Char
    toC = id
    fromC = id

instance Translate Double where
    type Tr Double = Double
    toC = id
    fromC = id

instance Translate LogDouble where
    type Tr LogDouble = LogDouble
    toC = id
    fromC = id

instance Translate Bool where
    type Tr Bool = Bool
    toC = id
    fromC = id

instance (Translate a, Translate b) => Translate (a->b) where
    type Tr (a->b) = Tr a -> Tr b

    fromC f = fromC . f . toC
    toC f = toC . f . fromC

instance Translate (IO a) where
    type Tr (IO a) = RealWorld -> a
    toC i = (\s -> let (_,x) = runIO i s in x)
    fromC i = makeIO i

instance Translate a => Translate [a] where
    type Tr [a] = EVector (Tr a)

    toC xs = listToVector $ map toC xs
    fromC v = map fromC $ vectorToList v

instance (Translate a, Translate b) => Translate (a, b) where
    type Tr (a, b) = EPair (Tr a) (Tr b)

    toC (x,y) = c_pair (toC x) (toC y)
    fromC xy = (fromC $ c_fst xy, fromC $ c_snd xy)

instance  {-# INCOHERENT #-} Translate String where
    type Tr String = CPPString

    toC = pack_cpp_string
    fromC = unpack_cpp_string
