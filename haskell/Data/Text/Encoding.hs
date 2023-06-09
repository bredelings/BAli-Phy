module Data.Text.Encoding where

import Data.ByteString
import Data.Text

data Decoding

data Utf8State

data StrictBuilder

data UnicodeException

-- OnDecodeError comes from Data.Text.Encoding.Error

-- N.B. Encoding/Decoding UTF-* shouldn't be that hard.
-- It doesn't require understanding Unicode code points, just how to represent them in a byte string.


{-
decodeLatin1 :: ByteString -> Text

decodeASCIIPrefix :: ByteString -> (Text, ByteString)

decodeUtf8Lenient :: ByteString -> Text

decodeUtf8' :: ByteString -> Either UnicodeException Text

decodeASCII' :: ByteString -> Maybe Text

decodeUtf8With :: OnDecodeError -> ByteString -> Text

decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text

decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text

streamDecodeUtf8With :: OnDecodeError -> ByteString -> Decoding

-- decodeUtf8More :: Utf8State -> ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)

startUtf8State :: Utf8State

-- strictBuilderToText :: StrictBuilder -> Text

-- textToStrictBuilder :: Text -> StrictBuilder

decodeASCII :: ByteString -> Text

decodeUtf16LE :: ByteString -> Text

decodeUtf16BE :: ByteString -> Text

decodeUtf32LE :: ByteString -> Text

decodeUtf32BE :: ByteString -> Text

streamDecodeUtf8 :: ByteString -> Decoding

encodeUtf8 :: Text -> ByteString

encodeUtf16LE :: Text -> ByteString

encodeUtf16BE :: Text -> ByteString

encodeUtf32LE :: Text -> ByteString

encodeUtf32BE :: Text -> ByteString

Builder comes from Data.Binary.Builder

-- encodeUtf8Builder :: Text -> Builder

-- encodeUtf8BuilderEscaped :: BoundedPrim Word8 -> Text -> Builder

validateUtf8Chunk :: ByteString -> (Int, Maybe Utf8State)

validateUtf8More :: Utf8State -> ByteString -> (Int, Maybe Utf8State)

-}
