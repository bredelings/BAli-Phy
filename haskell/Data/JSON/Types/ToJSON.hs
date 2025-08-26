module Data.JSON.Types.ToJSON where

import Data.JSON.Types.Internal
import qualified Data.JSON.Encoding as E
import Data.JSON.Encoding (Encoding, Encoding',Series {- , dict, emptyArray_ -} )

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.ByteString

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
    toEncodingList = toEncoding . toJSONList

    omitField :: a -> Bool
    omitField = const False

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

instance ToJSON Text where
    toJSON x = String x

    toEncoding = E.text

instance ToJSON Bool where
    toJSON x = Bool x
    toEncoding = E.bool

instance ToJSON Double where
    toJSON x = FNumber x

instance ToJSON Int where
    toJSON x = INumber x

instance ToJSON a => ToJSON [a] where
    toJSON x = toJSONList x

-- BUG: In instance for 'ToJSON (Map Compiler.Base.String a)' for type 'Map Compiler.Base.String a': Compiler.Base.String is not a type variable!
-- BUG: If we just replace String with b, it checks... but should it?
instance (ToJSONKey a, ToJSON b) => ToJSON (M.Map a b) where
    toJSON (M.Map xs) = object [(toJSONKey key, toJSON value) | (key, value) <- xs]
    -- toEncoding (M.Map xs) = ...

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (x,y) = Array [toJSON x, toJSON y]
    -- toEncoding (x,y) = ...

-- Apparently we need this so that we write
--    pi[A]=value  pi[C]=value
-- And not
--    pi[1][1]="A"  pi[1][2] = value  pi[2][1] ="C"  pi[2][2]=value
instance {-# INCOHERENT #-} ToJSON a => ToJSON ([Char],a) where
    toJSON (s,x) = Array [toJSON s, toJSON x]
    toJSONList xs = object [(toJSONKeyList key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (x,y,z) = Array [toJSON x, toJSON y, toJSON z]
    -- toEncoding (x,y,z) = ...


class KeyValue kv where
    type KVOut kv

    (.=) :: ToJSON v => Key -> v -> kv
    infixr 8 .=

    explicitToField :: (v-> KVOut kv) -> Key -> v -> kv

instance KeyValue Series where
    type KVOut Series = Encoding
    (.=) = explicitToField toEncoding
    explicitToField f name value = E.pair name (f value)


instance (key ~ Key, value ~ Value) => KeyValue (key, value) where
    type KVOut (key, value) = Value
    (.=) = explicitToField toJSON
    explicitToField f name value = (name, f value)
