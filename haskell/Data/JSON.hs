module Data.JSON (module Data.JSON.Types.Internal,
                  module Data.JSON.Types.ToJSON,
                  encode)
    where

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

