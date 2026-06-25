module Data.Text.Encoding
    (encodeUtf8,
     decodeUtf8)
    where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Foreign.String (CPPString)

data Decoding

data Utf8State

data StrictBuilder

data UnicodeException

-- OnDecodeError comes from Data.Text.Encoding.Error

-- N.B. Encoding/Decoding UTF-* shouldn't be that hard.
-- It doesn't require understanding Unicode code points, just how to represent them in a byte string.

-- Raw byte transport bridge used only to rewrap Text's validated UTF-8 storage
-- as ByteString.  It does not validate or interpret bytes.
foreign import ecall "ByteString:fromCPPStringBytes" fromCPPStringBytes :: CPPString -> ByteString

-- Raw byte transport bridge used only before Text.fromCppString performs UTF-8
-- validation while decoding.
foreign import ecall "ByteString:toCPPStringBytes" toCPPStringBytes :: ByteString -> CPPString

encodeUtf8 :: T.Text -> ByteString
encodeUtf8 = fromCPPStringBytes . T.toCppString

decodeUtf8 :: ByteString -> T.Text
decodeUtf8 = T.fromCppString . toCPPStringBytes


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
