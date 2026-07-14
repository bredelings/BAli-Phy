{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Enum
import Compiler.Error (error)
import Compiler.Num
import qualified Data.Array as A
import Data.Eq
import qualified Data.Foldable as F
import Data.Functor
import Data.Maybe
import Data.Ord
import Data.Traversable (traverse)
import System.IO (print, putStrLn)

data Box = Box Int

step old new = old * 10 + new

mapping index = if index == 0 then 0 else 2

-- Process duplicate accum associations from left to right and retain the
-- source bounds.
checkAccum = do
    putStrLn "accum"
    let base = A.listArray (5,5) [1] :: A.Array Int Int
        result = A.accum step base [(5,2),(5,3)]
    print (A.bounds result)
    print (result A.! 5)

-- Accumulate tuple indices in row-major storage while leaving an empty
-- array's initial value and combining function unevaluated.
checkAccumArray = do
    putStrLn "accumArray"
    let values = A.accumArray (+) 10 ((0,0),(1,1))
            [((0,1),1),((0,1),2),((1,0),5)]
            :: A.Array (Int,Int) Int
        empty = A.accumArray
            (\_ _ -> error "empty combine was forced")
            (error "empty initial value was forced") (2,1) []
            :: A.Array Int Int
    print (A.elems values)
    print (A.bounds empty)
    print (A.numElements empty)

-- Force a combining result only to WHNF without independently forcing its
-- inputs, lazy field, or an unrelated initial slot.
checkAccumLaziness = do
    putStrLn "accum laziness"
    let values = A.accumArray (\_ new -> Box new)
            (error "old value was forced") (0,1)
            [(0,error "new value was forced")] :: A.Array Int Box
    case values A.! 0 of
        Box _ -> print (1 :: Int)
    print (A.numElements values)

-- Check association reordering and last-write-wins replacement without
-- forcing the overwritten value.
checkAssociations = do
    putStrLn "associations"
    let values = A.array (-2,1)
            [(0,error "overwritten association was forced"),
             (1,40),(-2,10),(-1,20),(0,31)] :: A.Array Int Int
    print (A.bounds values)
    print (A.indices values)
    print (A.elems values)
    print (A.assocs values)
    print (values A.! 0)

-- Check stored bounds, sizes, and index enumeration without forcing an input
-- list when the requested range is empty.
checkBounds = do
    putStrLn "bounds"
    let positive = A.listArray (3,5) [30,40,50] :: A.Array Int Int
        negative = A.listArray (-2,1) [10,20,30,40] :: A.Array Int Int
        empty = A.listArray (2,1) (error "empty array input was forced") :: A.Array Int Int
    print (A.bounds positive)
    print (A.numElements positive)
    print (A.indices positive)
    print (A.bounds negative)
    print (A.numElements negative)
    print (A.indices negative)
    print (A.bounds empty)
    print (A.numElements empty)
    print (A.indices empty)
    print (A.elems empty)

-- Exercise array ordering, display, mapping, folding, and bounds-preserving
-- traversal in element order.
checkInstances = do
    putStrLn "instances"
    let values = A.listArray (2,4) [1,2,3] :: A.Array Int Int
        later = A.listArray (2,4) [1,2,4] :: A.Array Int Int
        shifted = A.listArray (3,5) [1,2,3] :: A.Array Int Int
        empty1 = A.listArray (2,1) [] :: A.Array Int Int
        empty2 = A.listArray (9,8) [] :: A.Array Int Int
        mapped = fmap (+10) values
    print values
    print (values == A.listArray (2,4) [1,2,3])
    print (compare values later == LT)
    print (values == shifted)
    print (empty1 == empty2)
    print (compare empty1 empty2 == EQ)
    print (A.bounds mapped)
    print (A.elems mapped)
    print (F.foldl (+) 0 values)
    print (F.foldr (:) [] values)
    print (traverse (\value -> Just (value + 1)) values
           :: Maybe (A.Array Int Int))

-- Map a source array into tuple target order while avoiding the mapping
-- function and source array for an empty target.
checkIxMap = do
    putStrLn "ixmap"
    let source = A.listArray (0,3) [10,20,30,40] :: A.Array Int Int
        mapped = A.ixmap ((0,0),(1,1)) (\(row,column) -> row * 2 + column) source
        empty = A.ixmap (2,1)
            (error "empty mapping was forced" :: Int -> Int)
            (error "empty source was forced" :: A.Array Int Int)
            :: A.Array Int Int
    print (A.bounds mapped)
    print (A.assocs mapped)
    print (A.bounds empty)
    print (A.numElements empty)

-- Leave an invalid mapped source index dormant when its target is unselected.
checkIxMapLaziness = do
    putStrLn "ixmap laziness"
    let source = A.listArray (0,1) [10,20] :: A.Array Int Int
        mapped = A.ixmap (0,1) mapping source :: A.Array Int Int
    print (mapped A.! 0)

-- Check short, excess, and empty list inputs while observing only elements
-- defined by the supplied prefix.
checkListArray = do
    putStrLn "listArray"
    let short = A.listArray (-1,1) [4,5] :: A.Array Int Int
        excess = A.listArray (5,6)
            [10,20,error "excess listArray element was forced"] :: A.Array Int Int
        heapBacked = A.listArray (0,10) [0,1,2,3,4,5,6,7,8,9,10]
            :: A.Array Int Int
        empty = A.listArray (1,0)
            (error "empty listArray input was forced") :: A.Array Int Int
    print (A.bounds short)
    print (A.numElements short)
    print (short A.! (-1))
    print (short A.! 0)
    print (A.elems excess)
    print (A.elems heapBacked)
    print (A.elems empty)

-- Build a recursive array whose lazy values refer to earlier array elements.
checkRecursive = do
    putStrLn "recursive"
    let values = A.array (0,3)
            [(0,1),
             (1,(values A.! 0) + 1),
             (2,(values A.! 1) + 1),
             (3,(values A.! 2) + 1)] :: A.Array Int Int
    print (A.elems values)
    print (values A.! 3)

-- Check that tuple ranges enumerate and index in row-major order.
checkTupleIndex = do
    putStrLn "tuple index"
    let values = A.listArray ((1,-1),(2,1)) [10..15]
            :: A.Array (Int,Int) Int
    print (A.bounds values)
    print (A.indices values)
    print (A.elems values)
    print (values A.! (1,1))
    print (values A.! (2,-1))
    print (A.assocs values)

-- Check bounds preservation, immutability, tuple updates, and last-write-wins
-- replacement without forcing an overwritten value.
checkUpdate = do
    putStrLn "update"
    let base = A.listArray (2,4) [10,20,30] :: A.Array Int Int
        updated = base A.// [(3,error "overwritten update was forced"),(2,11),(3,31)]
        tupleBase = A.listArray ((0,0),(1,1)) [1,2,3,4]
                :: A.Array (Int,Int) Int
        tupleUpdated = tupleBase A.// [((0,1),20)]
        empty = (A.listArray (2,1) [] :: A.Array Int Int) A.// []
    print (A.bounds updated)
    print (A.elems updated)
    print (A.elems base)
    print (A.elems tupleUpdated)
    print (A.bounds empty)

-- Updating one slot must not force unrelated base or replacement values.
checkUpdateLaziness = do
    putStrLn "update laziness"
    let base = A.array (0,1) [(0,error "base value was forced"),(1,2)]
            :: A.Array Int Int
        updated = base A.// [(0,error "replacement was forced"),(1,3)]
    print (updated A.! 1)

-- Run related successful array checks in one compiler process while keeping
-- expected-error and strictness-failure cases isolated.
main = do
    checkAccum
    checkAccumArray
    checkAccumLaziness
    checkAssociations
    checkBounds
    checkInstances
    checkIxMap
    checkIxMapLaziness
    checkListArray
    checkRecursive
    checkTupleIndex
    checkUpdate
    checkUpdateLaziness
