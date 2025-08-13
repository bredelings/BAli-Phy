module Data.JSON
    (
     encode,
     pairs,
     -- Core types
     Value(..),
     Encoding,
     fromEncoding,
     Array,
     Object,
     Key,
     -- Type conversion
     ToJSON(..),
     KeyValue(..),
     ToJSONKey(..),
     -- Constructors and accessors
     Series,
     object,
     pairs
    )
    where
-- TODO: implement FromJSON

import qualified Data.Text as T
import Data.Text (Text)
import Foreign.String
import Data.Semigroup
import Data.Monoid

import Data.JSON.Types.Internal
import Data.JSON.Types.ToJSON
import Data.JSON.Encoding
    
encode :: ToJSON a => a -> Text
encode = fromEncoding . toEncoding 

