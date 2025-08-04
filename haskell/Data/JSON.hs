module Data.JSON where

import qualified Data.Text as T
import Data.Text (Text)
import Foreign.String
import qualified Data.Map as M
import Data.ByteString

data CJSON

data EJSON

data Key = Key Text

instance Show Key where
    show (Key t) = show t

-- Hmm... it doesn't look like we can have a JSON object, just JSON representation, because a JSON object would have to have existential type fields.
data Value = Array [Value] | Object [(Key,Value)] | INumber Int | FNumber Double | Bool Bool | String Text | Null

-- BUG: No instance for 'Prelude.Show Compiler.Base.String' -- this is a mistake, because of type synonyms...
-- Probably we need to check_type( ) on constructor argument types...

instance Show Value where
    show Null = "null"
    show (INumber x) = show x
    show (FNumber x) = show x
    show (Bool x) = show x
    show (String x) = show $ T.unpack x
    show (Array x) = "["++intercalate "," (map show x) ++ "]"
    show (Object x) = "{"++ intercalate ", " [show key ++ ": "++ show value | (key,value) <- x] ++ "}"


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
    toJSON (M.Map xs) = Object [(toJSONKey key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (x,y) = Array [toJSON x, toJSON y]

instance {-# INCOHERENT #-} ToJSON a => ToJSON ([Char],a) where
    toJSON (s,x) = Array [toJSON s, toJSON x]
    toJSONList xs = Object [(toJSONKeyList key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (x,y,z) = Array [toJSON x, toJSON y, toJSON z]


-- backward compatibility
to_json = toJSON

foreign import bpcall "Foreign:c_json" builtin_c_json :: EJSON -> CJSON
c_json = builtin_c_json . deep_eval_json

foreign import bpcall "Foreign:" ejson_array   :: EVector EJSON -> EJSON
foreign import bpcall "Foreign:" ejson_object  :: EVector (EPair CPPString EJSON) -> EJSON
foreign import bpcall "Foreign:" ejson_inumber :: Int -> EJSON
foreign import bpcall "Foreign:" ejson_fnumber :: Double -> EJSON
foreign import bpcall "Foreign:" ejson_string  :: CPPString -> EJSON
foreign import bpcall "Foreign:" ejson_bool    :: Bool -> EJSON
foreign import bpcall "Foreign:" ejson_null    :: () -> EJSON

foreign import bpcall "Foreign:" cjson_to_bytestring :: CJSON -> CPPString

cjsonToText :: CJSON -> Text
cjsonToText = T.fromCppString . cjson_to_bytestring

jsonToText :: Value -> Text
jsonToText = cjsonToText . c_json

encode :: ToJSON a => a -> ByteString
encode = toEncoding          

deep_eval_json :: Value -> EJSON
deep_eval_json (Array xs)  = ejson_array $ toVector $ map deep_eval_json xs
deep_eval_json (Object xs) = ejson_object $ toVector [c_pair (T.toCppString key) (deep_eval_json value) | (Key key, value) <- xs]
deep_eval_json (INumber i)  = ejson_inumber i
deep_eval_json (FNumber f)  = ejson_fnumber f
deep_eval_json (Bool b)    = ejson_bool b
deep_eval_json (String s)  = ejson_string (T.toCppString s)
deep_eval_json Null        = ejson_null ()
