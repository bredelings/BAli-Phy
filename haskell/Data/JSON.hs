module Data.JSON where

import Data.Text as T
import Foreign.String
import qualified Data.Map as M

data CJSON

data EJSON

data Key = Key Text

instance Show Key where
    show (Key t) = show t

foreign import bpcall "Foreign:c_json" builtin_c_json :: EJSON -> CJSON

-- Hmm... it doesn't look like we can have a JSON object, just JSON representation, because a JSON object would have to have existential type fields.
data JSON = Array [JSON] | Object [(Key,JSON)] | Number Double | Bool Bool | String Text | Null

-- BUG: No instance for 'Prelude.Show Compiler.Base.String' -- this is a mistake, because of type synonyms...
-- Probably we need to check_type( ) on constructor argument types...

instance Show JSON where
    show Null = "null"
    show (Number x) = show x
    show (Bool x) = show x
    show (String x) = show $ unpack x
    show (Array x) = "["++intercalate "," (map show x) ++ "]"
    show (Object x) = "{"++ intercalate ", " [show key ++ ": "++ show value | (key,value) <- x] ++ "}"


class ToJSONKey a where
    toJSONKey :: a -> Key
    toJSONKeyList :: [a] -> Key
    toJSONKeyList s = error "toJSONKeyList: not implemented for this type"

instance ToJSONKey Text where
    toJSONKey s = Key s

instance ToJSONKey Char where
    toJSONKey c = Key $ pack [c]
    toJSONKeyList s = Key $ pack s

instance ToJSONKey a => ToJSONKey [a] where
    toJSONKey l = toJSONKeyList l

class ToJSON a where
    toJSON :: a -> JSON
    toJSONList :: [a] -> JSON

    toJSONList x = Array $ map toJSON x

instance ToJSON () where
    toJSON () = Null

instance ToJSON Char where
    toJSON c = String (pack [c])
    toJSONList s = String (pack s)

instance ToJSON Text where
    toJSON x = String x

instance ToJSON Bool where
    toJSON x = Bool x

instance ToJSON Double where
    toJSON x = Number x

instance ToJSON Int where
    toJSON x = Number (intToDouble x)

instance ToJSON a => ToJSON [a] where
    toJSON x = toJSONList x

-- BUG: In instance for 'Data.JSON.ToJSON (Data.Map.Map Compiler.Base.String a)' for type 'Data.Map.Map Compiler.Base.String a': Compiler.Base.String is not a type variable!
-- BUG: If we just replace String with b, it checks... but should it?
instance (ToJSONKey a, ToJSON b) => ToJSON (M.Map a b) where
    toJSON (M.Map xs) = Object [(toJSONKey key, toJSON value) | (key, value) <- xs]

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (x,y) = Array [toJSON x, toJSON y]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (x,y,z) = Array [toJSON x, toJSON y, toJSON z]


-- backward compatibility
to_json = toJSON


foreign import bpcall "Foreign:" ejson_array  :: EVector EJSON -> EJSON
foreign import bpcall "Foreign:" ejson_object :: EVector (EPair CPPString EJSON) -> EJSON
foreign import bpcall "Foreign:" ejson_number :: Double -> EJSON
foreign import bpcall "Foreign:" ejson_string :: CPPString -> EJSON
foreign import bpcall "Foreign:" ejson_bool   :: Bool -> EJSON
foreign import bpcall "Foreign:" ejson_null   :: () -> EJSON


deep_eval_json (Array xs)  = ejson_array $ list_to_vector $ map deep_eval_json xs
deep_eval_json (Object xs) = ejson_object $ list_to_vector [c_pair key (deep_eval_json value) | (Key (Text key), value) <- xs]
deep_eval_json (Number n)  = ejson_number n
deep_eval_json (Bool b)    = ejson_bool b
deep_eval_json (String (Text s))  = ejson_string s
deep_eval_json Null        = ejson_null ()

c_json = builtin_c_json . deep_eval_json
