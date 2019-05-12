module Data.JSON where

import Data.Text as T
import Foreign.String

builtin c_json 1 "c_json" "Foreign"

-- Hmm... it doesn't look like we can have a JSON object, just JSON representation, because a JSON object would have to have existential type fields.
data JSON = Array [JSON] | Object [(String,JSON)] | Number a | Bool Bool | String CppString | Null

json_to_string (Array x) = "["++intercalate "," (map json_to_string x) ++ "]"
-- we aren't escaping strings here...
-- if we actually build a C++ json object we could print that
json_to_string (Object x) = "{"++ intercalate ", " ["\""++key++"\": "++json_to_string value | (key,value) <- x] ++ "}"
json_to_string (Number x) = show x
json_to_string (Bool True) = "true"
json_to_string (Bool False) = "false"
json_to_string (String s) = "\""++unpack_cpp_string s++"\""
json_to_string (Null) = "null"

is_non_empty_string (c:cs) | is_char c = True
is_non_empty_string _ = False
to_json s@(c:_) | is_char c = String (pack_cpp_string s)
to_json (Text s)            = String s
to_json []                  = Array []
to_json o@((key,value):kvs) | is_non_empty_string key = Object [(key,to_json value) | (key, value) <- o]
to_json l@(_:_)             = Array [to_json x | x <- l]
to_json x | is_double x     = Number x
          | is_int    x     = Number x
to_json True                = Bool True
to_json False               = Bool False
to_json (x,y)               = Array [to_json x, to_json y]
to_json (x,y,z)             = Array [to_json x, to_json y, to_json z]
to_json (x,y,z,w)           = Array [to_json x, to_json y, to_json z, to_json w]
to_json _                   = Null
