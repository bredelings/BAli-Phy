module Data.BitVector where

data BitVector = BitVector ()

builtin builtin_zeros 1 "empty_bitvector" "Bits"

-- see Boost.Multiprecision

-- from Data.Bits
-- (BitVector x) .&. (BitVector y) = BitVector (builtin_bitwise_and x y)
-- (BitVector y) .|. (BitVector y) = BitVector (builtin_bitwise_or  x y)
-- (BitVector x) `xor` (BitVector y) = BitVector (builting_bitwise_xor x y)
-- complement (BitVector x)        = BitVector (builtin_complement x)
-- popcount (BitVector x)          = builtin_popcount x
-- bitsize (BitVector x)           = builtin_bitsize x
-- any?
-- none?
                                  
-- end Data.Bits

--zeros n = 0          -- a bitvector of n zeros
--ones  n = 1          -- a bitvector of n ones
--bitVec i = i         -- a bitvector created from integer i

--size x = 0           -- how many bits in the vector
--width x = size x

--nat x = 0            -- convert the bitvector to an Integer (large)
--uint x = nat x

--int x = 0            -- convert the bitvector to an integer using 2s-complement

--x ==. y = 0          -- equality assuming same size

--x /=. y = 0          -- inequality assuming same size

--x @. i = 0           -- test bit i

--x @@ (i1,i2)  = 0    -- u @@ (j,i) == fromBits (map (u @.) [j,j-1..i])

--x !. i = 0           -- index from the end of the sequence

--x # y = 0            -- concatenate 2 bit vectors

-- not_ x = complement x  -- complement


-- fromBits :: [Bool] -> BV
-- fromBits [x] = 0     -- fromBits [False, False, True] -> [3]1

-- toBits x = []        -- toBits


