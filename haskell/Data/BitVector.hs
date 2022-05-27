module Data.BitVector where

data BitVector = BitVector ()
data CBitVector = CBitVector

builtin builtin_zeros 1 "Bits:empty_bitvector"
builtin builtin_complement 1 "Bits:complement"
builtin builtin_bitwise_or 2 "Bits:bitwise_or"
builtin builtin_bitwise_and 2 "Bits:bitwise_and"
builtin builtin_bitwise_xor 2 "Bits:bitwise_xor"
builtin builtin_eq 2 "Bits:eq"
builtin builtin_neq 2 "Bits:neq"
builtin builtin_test_bit 2 "Bits:test_bit"
builtin builtin_set_bit 2 "Bits:set_bit"
builtin builtin_clear_bit 2 "Bits:clear_bit"
builtin builtin_size 1 "Bits:size"
builtin builtin_popcount 1 "Bits:popcount"

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
--fromBits bits = builtin_from_bits $ list_to_vector bits

-- toBits x = []        -- toBits
toBits x = reverse [ x @. n | n <- [0..size x - 1] ]

