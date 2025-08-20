module Data.JSON.Types.Internal where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String    
    
data Key = Key Text

instance Show Key where
    show (Key t) = show t

instance IsString Key where
    fromString s = Key $ T.pack s

type Pair = (Key, Value)

type Object = [Pair]

type Array = [Value]
                   
data Value = Array Array | Object Object | INumber Int | FNumber Double | Bool Bool | String Text | Null

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

