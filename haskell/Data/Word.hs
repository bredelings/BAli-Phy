module Data.Word where

type Word = Int

-- We need to fix this.
-- Char should really be 32 bits and hold a unicode code point.
-- Word8 should be something like Word8 Word8#
type Word8 = Char
