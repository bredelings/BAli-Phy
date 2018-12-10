module Data.JSON where

-- Hmm... it doesn't look like we can have a JSON object, just JSON representation, because a JSON object would have to have existential type fields.
data JSON = JSONArray [JSON] | JSONObject [(String,JSON)] | JSONDouble Double | JSONInt Int | JSONBool Bool | JSONString String | JSONNull

json_to_string (JSONArray x) = "["++intercalate "," (map json_to_string x) ++ "]"
json_to_string (JSONObject x) = "{"++ intercalate ", " ["\""++key++"\": "++json_to_string value | (key,value) <- x] ++ "}"
json_to_string (JSONDouble x) = show x
json_to_string (JSONInt x) = show x
json_to_string (JSONBool True) = "true"
json_to_string (JSONBool False) = "false"
json_to_string (JSONString x) = "\""++x++"\""
json_to_string (JSONNull) = "null"

to_json s@(c:_) | is_char = JSONString s
to_json []               = JSONArray []
to_json l@(_:_)          = JSONArray [to_json x | x <- l]
to_json x | is_double x  = JSONDouble x
          | is_int    x  = JSONInt x
to_json True             = JSONBool True
to_json False            = JSONBool False
to_json _                = JSONNull
