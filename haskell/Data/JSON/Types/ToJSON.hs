{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.JSON.Types.ToJSON where

import Data.JSON.Types.Internal
import qualified Data.JSON.Encoding as E
import Data.JSON.Encoding (Encoding, Encoding',Series {- , dict, emptyArray_ -} )

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M
import Data.ByteString
import Data.Semigroup
import Data.Monoid
import qualified Data.JSON.Encoding as E
import Data.JSON.Encoding ( (>*<) )

class ToJSONKey a where
    toJSONKey :: a -> Key
    toJSONKeyList :: [a] -> Key
    toJSONKeyList s = error "toJSONKeyList: not implemented for this type"

instance ToJSONKey Key where
    toJSONKey s = s

instance ToJSONKey Text where
    toJSONKey s = Key s

instance ToJSONKey Char where
    toJSONKey c = Key $ T.pack [c]
    toJSONKeyList s = Key $ T.pack s

instance ToJSONKey a => ToJSONKey [a] where
    toJSONKey l = toJSONKeyList l

class ToJSON a where
    toJSON :: a -> Value

    toJSONList :: [a] -> Value
    toJSONList x = Array $ map toJSON x

    toEncoding :: a -> Encoding
    toEncoding = toEncoding . toJSON

    toEncodingList :: [a] -> Encoding
    toEncodingList = listEncoding toEncoding

    omitField :: a -> Bool
    omitField = const False

listEncoding = E.list
                
instance ToJSON Value where
    toJSON = id

    toEncoding = E.value

instance ToJSON () where
    toJSON () = Null
    toEncoding () = E.text $ T.pack $ "()"

instance ToJSON Char where
    toJSON c = String (T.pack [c])
    toJSONList s = String (T.pack s)

    toEncoding c = E.text $ T.pack $ [c]
    toEncodingList s = E.text $ T.pack s

instance ToJSON Text where
    toJSON x = String x
    toEncoding = E.text

instance ToJSON Bool where
    toJSON x = Bool x
    toEncoding = E.bool

instance ToJSON Double where
    toJSON x = FNumber x
    toEncoding x = E.double x

instance ToJSON Int where
    toJSON x = INumber x
    toEncoding x = E.int x

instance ToJSON a => ToJSON [a] where
    toJSON x = toJSONList x
    toEncoding x = toEncodingList x

instance (ToJSONKey a, ToJSON b) => ToJSON (M.Map a b) where
    toJSON m = object [(toJSONKey key, toJSON value) | (key, value) <- M.toAscList m]
    toEncoding m = E.pairs (foldr (<>) mempty [toJSONKey k .= v | (k,v) <- M.toAscList m])

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (x,y) = Array [toJSON x, toJSON y]
    toEncoding (x,y) = E.wrapArray $ toEncoding x >*< toEncoding y

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (x,y,z) = Array [toJSON x, toJSON y, toJSON z]
    toEncoding (x,y,z) = E.wrapArray $ toEncoding x >*< toEncoding y >*< toEncoding z


class KeyValue e kv | kv -> e where
    (.=) :: ToJSON v => Key -> v -> kv
    infixr 8 .=

    explicitToField :: (v -> e) -> Key -> v -> kv

instance KeyValue Encoding Series where
    (.=) = explicitToField toEncoding
    explicitToField f name value = E.pair name (f value)


instance (key ~ Key, value ~ Value) => KeyValue Value (key, value) where
    (.=) = explicitToField toJSON
    explicitToField f name value = (name, f value)

instance ToJSON (U.Vector Int) where
    toJSON values = Array (map toJSON (U.toList values))
    toEncoding = E.intVector

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d) => ToJSON (a,b,c,d) where
    toJSON (x1,x2,x3,x4) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4]
    toEncoding (x1,x2,x3,x4) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e) => ToJSON (a,b,c,d,e) where
    toJSON (x1,x2,x3,x4,x5) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5]
    toEncoding (x1,x2,x3,x4,x5) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f) => ToJSON (a,b,c,d,e,f) where
    toJSON (x1,x2,x3,x4,x5,x6) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6]
    toEncoding (x1,x2,x3,x4,x5,x6) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g) => ToJSON (a,b,c,d,e,f,g) where
    toJSON (x1,x2,x3,x4,x5,x6,x7) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7]
    toEncoding (x1,x2,x3,x4,x5,x6,x7) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h) => ToJSON (a,b,c,d,e,f,g,h)
    where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i) => ToJSON
    (a,b,c,d,e,f,g,h,i) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j) => ToJSON
    (a,b,c,d,e,f,g,h,i,j) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j,ToJSON k) =>
    ToJSON (a,b,c,d,e,f,g,h,i,j,k) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10, toJSON x11]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10 >*<
         toEncoding x11)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j,ToJSON k,ToJSON
    l) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10, toJSON x11, toJSON x12]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10 >*<
         toEncoding x11 >*<
         toEncoding x12)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j,ToJSON k,ToJSON
    l,ToJSON m) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10, toJSON x11, toJSON x12, toJSON x13]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10 >*<
         toEncoding x11 >*<
         toEncoding x12 >*<
         toEncoding x13)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j,ToJSON k,ToJSON
    l,ToJSON m,ToJSON n) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10, toJSON x11, toJSON x12, toJSON x13, toJSON x14]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10 >*<
         toEncoding x11 >*<
         toEncoding x12 >*<
         toEncoding x13 >*<
         toEncoding x14)

instance (ToJSON a,ToJSON b,ToJSON c,ToJSON d,ToJSON e,ToJSON f,ToJSON g,ToJSON h,ToJSON i,ToJSON j,ToJSON k,ToJSON
    l,ToJSON m,ToJSON n,ToJSON o) => ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toJSON (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) =
        Array [toJSON x1, toJSON x2, toJSON x3, toJSON x4, toJSON x5, toJSON x6, toJSON x7, toJSON x8, toJSON x9,
        toJSON x10, toJSON x11, toJSON x12, toJSON x13, toJSON x14, toJSON x15]
    toEncoding (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) = E.wrapArray
        (toEncoding x1 >*<
         toEncoding x2 >*<
         toEncoding x3 >*<
         toEncoding x4 >*<
         toEncoding x5 >*<
         toEncoding x6 >*<
         toEncoding x7 >*<
         toEncoding x8 >*<
         toEncoding x9 >*<
         toEncoding x10 >*<
         toEncoding x11 >*<
         toEncoding x12 >*<
         toEncoding x13 >*<
         toEncoding x14 >*<
         toEncoding x15)
