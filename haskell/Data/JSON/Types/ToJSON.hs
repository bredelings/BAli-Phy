module Data.JSON.Types.ToJSON where

import Data.JSON.Types.Internal

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.ByteString

class ToJSONKey a where
    toJSONKey :: a -> Key
    toJSONKeyList :: [a] -> Key
    toJSONKeyList s = error "toJSONKeyList: not implemented for this type"

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

    toEncoding :: a -> ByteString
    toEncoding = jsonToText . toJSON

    toEncodingList :: [a] -> ByteString
    toEncodingList = jsonToText . toJSONList

    omitField :: a -> Bool
    omitField = const False

instance ToJSON Value where
    toJSON = id

instance ToJSON () where
    toJSON () = Null

instance ToJSON Char where
    toJSON c = String (T.pack [c])
    toJSONList s = String (T.pack s)

instance ToJSON Text where
    toJSON x = String x

instance ToJSON Bool where
    toJSON x = Bool x

instance ToJSON Double where
    toJSON x = FNumber x

instance ToJSON Int where
    toJSON x = INumber x

instance ToJSON a => ToJSON [a] where
    toJSON x = toJSONList x

-- BUG: In instance for 'Data.JSON.ToJSON (Data.Map.Map Compiler.Base.String a)' for type 'Data.Map.Map Compiler.Base.String a': Compiler.Base.String is not a type variable!
-- BUG: If we just replace String with b, it checks... but should it?
instance (ToJSONKey a, ToJSON b) => ToJSON (M.Map a b) where
    toJSON (M.Map xs) = object [(toJSONKey key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (x,y) = Array [toJSON x, toJSON y]

instance {-# INCOHERENT #-} ToJSON a => ToJSON ([Char],a) where
    toJSON (s,x) = Array [toJSON s, toJSON x]
    toJSONList xs = object [(toJSONKeyList key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (x,y,z) = Array [toJSON x, toJSON y, toJSON z]


encode :: ToJSON a => a -> ByteString
encode = toEncoding          

