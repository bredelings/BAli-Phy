module Data.ByteString where

import Data.Word
import Data.Text    

-- FIXME-UNICODE: Compatibility alias.  ByteString should be a raw byte type,
-- not validated UTF-8 Text, before Data.Text.Encoding is implemented.
type ByteString = Text
