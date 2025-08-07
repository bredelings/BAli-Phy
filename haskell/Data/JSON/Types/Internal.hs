module Data.JSON.Types.Internal where

import qualified Data.Text as T
import Data.Text (Text)
    
data Key = Key Text

instance Show Key where
    show (Key t) = show t

data Value = Array [Value] | Object [(Key,Value)] | INumber Int | FNumber Double | Bool Bool | String Text | Null

object = Object

-- Hmm... it doesn't look like we can have a JSON object, just JSON representation, because a JSON object would have to have existential type fields.


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


data CJSON

data EJSON

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

deep_eval_json :: Value -> EJSON
deep_eval_json (Array xs)  = ejson_array $ toVector $ map deep_eval_json xs
deep_eval_json (Object xs) = ejson_object $ toVector [c_pair (T.toCppString key) (deep_eval_json value) | (Key key, value) <- xs]
deep_eval_json (INumber i) = ejson_inumber i
deep_eval_json (FNumber f) = ejson_fnumber f
deep_eval_json (Bool b)    = ejson_bool b
deep_eval_json (String s)  = ejson_string (T.toCppString s)
deep_eval_json Null        = ejson_null ()

