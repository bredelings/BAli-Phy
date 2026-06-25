module Data.ByteString
    (ByteString,
     empty, null, length, index,
     pack, unpack,
     take, drop, append)
    where

import Prelude hiding (null, length, take, drop, empty)
import Data.Eq
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Word
import Foreign.Vector

data ByteString

foreign import ecall "ByteString:empty" empty :: ByteString

foreign import ecall "ByteString:length" length :: ByteString -> Int

foreign import ecall "ByteString:index" index :: ByteString -> Int -> Word8

foreign import ecall "ByteString:pack" packRaw :: EVector Word8 -> ByteString
pack :: [Word8] -> ByteString
pack = packRaw . toVector

foreign import ecall "ByteString:unpack" unpackRaw :: ByteString -> EVector Word8
unpack :: ByteString -> [Word8]
unpack = vectorToList . unpackRaw

null :: ByteString -> Bool
null bs = length bs == 0

foreign import ecall "ByteString:take" takeRaw :: Int -> ByteString -> ByteString
take :: Int -> ByteString -> ByteString
take n bs
    | n <= 0    = empty
    | n >= len  = bs
    | otherwise = takeRaw n bs
    where len = length bs

foreign import ecall "ByteString:drop" dropRaw :: Int -> ByteString -> ByteString
drop :: Int -> ByteString -> ByteString
drop n bs
    | n <= 0    = bs
    | n >= len  = empty
    | otherwise = dropRaw n bs
    where len = length bs

foreign import ecall "ByteString:append" append :: ByteString -> ByteString -> ByteString

foreign import ecall "ByteString:equals" equalsRaw :: ByteString -> ByteString -> Bool

instance Eq ByteString where
    (==) = equalsRaw

instance Semigroup ByteString where
    (<>) = append

instance Monoid ByteString where
    mempty = empty
