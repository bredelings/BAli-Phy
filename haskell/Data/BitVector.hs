module Data.BitVector where

data CBitVector = CBitVector
data BitVector = BitVector CBitVector

foreign import bpcall "Bits:empty_bitvector" builtin_zeros :: Int -> CBitVector
foreign import bpcall "Bits:complement" builtin_complement :: CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_or" builtin_bitwise_or :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_and" builtin_bitwise_and :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_xor" builtin_bitwise_xor :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:eq" builtin_eq :: CBitVector -> CBitVector -> Bool
foreign import bpcall "Bits:neq" builtin_neq :: CBitVector -> CBitVector -> Bool
foreign import ecall  "Bits:test_bit" builtin_test_bit :: CBitVector -> Int -> Bool
foreign import bpcall "Bits:set_bit" builtin_set_bit :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:clear_bit" builtin_clear_bit :: CBitVector -> Int -> CBitVector
foreign import ecall "Bits:size" builtin_size :: CBitVector -> Int
foreign import bpcall "Bits:popcount" builtin_popcount :: CBitVector -> Int

-- see Boost.Multiprecision

-- Fix module.cc:haskell_name_path( )

-- from Data.Bits
(BitVector x) .&.   (BitVector y) = BitVector (builtin_bitwise_and x y)
(BitVector x) .|.   (BitVector y) = BitVector (builtin_bitwise_or  x y)
(BitVector x) `xor` (BitVector y) = BitVector (builtin_bitwise_xor x y)
complement (BitVector x)        = BitVector (builtin_complement x)
popcount (BitVector x)          = builtin_popcount x
testBit (BitVector x) n         = builtin_test_bit x n
setBit (BitVector x) n          = BitVector $ builtin_set_bit x n
clearBit (BitVector x) n         = BitVector $ builtin_clear_bit x n
-- bitsize (BitVector x)           = builtin_bitsize x
-- any?
-- none?
                                  
-- end Data.Bits

zeros n = BitVector $ builtin_zeros n

ones  n = complement $ zeros n
--bitVec i = i         -- a bitvector created from integer i

size (BitVector x) = builtin_size x
width x = size x

--nat x = 0            -- convert the bitvector to an Integer (large)
--uint x = nat x

--int x = 0            -- convert the bitvector to an integer using 2s-complement

-- This is a hack since I can't define (==) and (/=.) without type classes.
(BitVector x) ==. (BitVector y) = builtin_eq x y  -- assumes size is the same

(BitVector x) /=. (BitVector y) = builtin_neq x y -- assumes size is the same

x @. i = testBit x i

--x @@ (i1,i2)  = 0    -- u @@ (j,i) == fromBits (map (u @.) [j,j-1..i])

-- x !. i = x @. (size x - i - 1)

--x # y = 0            -- concatenate 2 bit vectors

not_ x = complement x

-- fromBits :: [Bool] -> BitVector
--fromBits bits = builtin_from_bits $ toVector bits

-- toBits x = []        -- toBits
toBits x = reverse [ x @. n | n <- [0..size x - 1] ]

