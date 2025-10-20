module Data.JSON.Types.Foreign where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Data.JSON.Types.Internal    
import Data.JSON.Types.ToJSON
    
data CJSON

data EJSON

foreign import bpcall "Foreign:c_json" builtin_c_json :: EJSON -> CJSON

toCJSON :: ToJSON v => v -> CJSON
toCJSON = builtin_c_json . deep_eval_json . toJSON

foreign import bpcall "Foreign:" ejson_array   :: EVector EJSON -> EJSON
foreign import bpcall "Foreign:" ejson_object  :: EVector (EPair CPPString EJSON) -> EJSON
foreign import bpcall "Foreign:" ejson_inumber :: Int -> EJSON
foreign import bpcall "Foreign:" ejson_fnumber :: Double -> EJSON
foreign import bpcall "Foreign:" ejson_string  :: CPPString -> EJSON
foreign import bpcall "Foreign:" ejson_bool    :: Bool -> EJSON
foreign import bpcall "Foreign:" ejson_null    :: EJSON

foreign import bpcall "Foreign:" cjson_to_bytestring :: CJSON -> CPPString

cjsonToText :: CJSON -> Text
cjsonToText = T.fromCppString . cjson_to_bytestring

-- QUESTION: Is this faster than "encode"?
--jsonToText :: Value -> Text
--jsonToText = cjsonToText . toCJSON

deep_eval_json :: Value -> EJSON
deep_eval_json (Array xs)  = ejson_array $ toVector $ map deep_eval_json xs
deep_eval_json (Object xs) = ejson_object $ toVector [c_pair (T.toCppString key) (deep_eval_json value) | (Key key, value) <- xs]
deep_eval_json (INumber i) = ejson_inumber i
deep_eval_json (FNumber f) = ejson_fnumber f
deep_eval_json (Bool b)    = ejson_bool b
deep_eval_json (String s)  = ejson_string (T.toCppString s)
deep_eval_json Null        = ejson_null

