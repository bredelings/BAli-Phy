{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Enum
import Compiler.Error (error)
import Compiler.Fractional
import Compiler.Num
import Data.Bool (Bool(False, True))
import Data.Eq
import qualified Data.Foldable as F
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord
import Data.Traversable (traverse)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO (print, putStrLn)

-- Report a failed optional unboxed-vector lookup with a value outside the test
-- vectors' range.
maybeIndex values index = case values U.!? index of
    Nothing -> -1
    Just value -> value

boolInt :: Bool -> Int
boolInt False = 0
boolInt True = 1

-- Exercise construction of boxed vectors from the basic public builders.
boxedConstruction = do
    putStrLn "boxed construction"
    print (V.toList (V.empty :: V.Vector Int))
    print (V.toList (V.singleton 4 :: V.Vector Int))
    print (V.toList (V.replicate 3 7 :: V.Vector Int))
    print (V.toList (V.replicate 11 8 :: V.Vector Int))
    print (V.toList (V.fromList [0,1,2,3,4,5,6,7,8,9] :: V.Vector Int))
    print (V.toList (V.fromList [0,1,2,3,4,5,6,7,8,9,10] :: V.Vector Int))
    print (V.length (V.fromList [10,20,30] :: V.Vector Int))

-- Exercise generation and the two indexed and unindexed map operations.
boxedGeneration = do
    putStrLn "boxed generation"
    let values = V.generate 5 (\index -> index * index)
    print (V.toList values)
    print (V.toList (V.map (\value -> value + 1) values))
    print (V.toList (V.imap (\index value -> index + value) values))

-- Exercise the conventional boxed-vector instances without converting first.
boxedInstances = do
    putStrLn "boxed instances"
    let values = V.fromList [1,2,3] :: V.Vector Int
    print values
    print (values == V.fromList [1,2,3])
    print (compare values (V.fromList [1,2,4]) == LT)
    print (F.foldl (+) 0 values)
    print (F.foldr (:) [] values)
    print (traverse (\value -> Just (value + 1)) values :: Maybe (V.Vector Int))

-- Verify that shape operations and unselected elements retain boxed-vector
-- element laziness.
boxedLazyElements = do
    putStrLn "boxed lazy elements"
    let values = V.fromList [7, error "unselected vector element was forced", 9] :: V.Vector Int
        replicated = V.replicate 11 (error "replicated value was forced") :: V.Vector Int
        zero = V.replicate 0 (error "zero-length value was forced") :: V.Vector Int
        full = V.slice 0 (V.length values) values
        leftEmpty = V.empty V.++ values
        rightEmpty = values V.++ V.empty
    print (values V.! 0)
    print (V.length values)
    print (V.toList (V.take 1 values))
    print (V.length replicated)
    print (V.toList zero)
    print (V.length full, full V.! 0, leftEmpty V.! 0, rightEmpty V.! 2)

-- Exercise early termination and accumulator laziness in every indexed
-- boxed-vector loop whose cached length must not force elements.
boxedLoopLaziness = do
    putStrLn "boxed loop laziness"
    print (F.foldr (\value _ -> value) 0
        (V.fromList [11,error "foldr tail was forced"] :: V.Vector Int))
    print (F.foldl (\_ value -> value) (error "foldl initial was forced")
        (V.fromList [error "foldl intermediate was forced",22] :: V.Vector Int))
    print (F.foldl1 (\_ value -> value)
        (V.fromList [error "foldl1 first value was forced",33] :: V.Vector Int))
    print (F.foldr1 (\value _ -> value)
        (V.fromList [44,error "foldr1 last value was forced"] :: V.Vector Int))
    print (V.elemIndex 55
        (V.fromList [55,error "elemIndex continued after a match"] :: V.Vector Int))
    print
        ((V.fromList [error "equality forced an element"] :: V.Vector Int) == V.empty,
         compare
            (V.fromList [1,error "left ordering tail was forced"] :: V.Vector Int)
            (V.fromList [2,error "right ordering tail was forced"] :: V.Vector Int) == LT,
         compare
            (V.fromList [1] :: V.Vector Int)
            (V.fromList [1,error "longer ordering tail was forced"] :: V.Vector Int) == LT)

-- Exercise checked views, clamped take/drop, and boxed-vector append.
boxedSlicing = do
    putStrLn "boxed slicing"
    let values = V.fromList [0,1,2,3,4] :: V.Vector Int
    print (V.toList (V.slice 1 3 values))
    print (V.toList (V.take 3 values))
    print (V.toList (V.take (-1) values))
    print (V.toList (V.drop 2 values))
    print (V.toList (V.drop 20 values))
    print (V.toList (V.take 2 values V.++ V.drop 3 values))
    print (V.toList (V.slice 0 (V.length values) values),
           V.toList (V.take 20 values),
           V.toList (V.drop 0 values))
    print (V.toList (V.empty V.++ values),
           V.toList (values V.++ V.empty),
           V.toList (V.empty V.++ (V.empty :: V.Vector Int)))

-- Exercise primitive and nested-pair construction, indexing, conversion, and
-- the conventional immutable unboxed-vector instances.
unboxedBasic = do
    putStrLn "unboxed basic"
    print (U.toList (U.empty :: U.Vector Int))
    print (U.toList (U.singleton 4 :: U.Vector Int))
    print (U.toList (U.replicate 3 7 :: U.Vector Int))
    let ints = U.fromList [0,1,2,3,4] :: U.Vector Int
        doubles = U.fromList [1.5,2.5] :: U.Vector Double
        nested = U.fromList [((1,1.5),10),((2,2.5),20)] ::
            U.Vector ((Int,Double),Int)
    print (U.toList ints)
    print (U.toList doubles)
    print (U.length ints, boolInt (U.null ints),
           boolInt (U.null (U.empty :: U.Vector Int)))
    print (ints U.! 2, maybeIndex ints 3, maybeIndex ints (-1),
           maybeIndex ints 99)
    print (boolInt (ints == U.fromList [0,1,2,3,4]),
           boolInt (ints < U.fromList [0,1,3]))
    print ints
    print (U.toList nested)
    let moderate = U.fromList [0..999] :: U.Vector Int
    print (U.length moderate, moderate U.! 0, moderate U.! 999)

-- Verify that stable shape metadata and unboxed-vector view construction do
-- not evaluate numeric heads or force the lazy native owner.
unboxedShapeLaziness = do
    putStrLn "unboxed shape laziness"
    let values = U.fromList [error "unboxed vector head forced", 2] :: U.Vector Int
    print (U.length values, boolInt (U.null values))
    print (U.length (U.slice 0 1 values))
    print (U.length (U.take 1 values), U.length (U.drop 1 values))
    print (boolInt (U.null (U.slice 1 0 values)))
    print (U.length (U.replicate 0 (error "pair value forced")
                    :: U.Vector (Int,Int)))
    print (U.length (U.replicate 2 (error "nonempty pair value forced")
                    :: U.Vector (Int,Int)))
    print (U.length (U.replicate 1 (error "nested pair value forced")
                    :: U.Vector ((Int,Double),(Int,Int))))

-- Exercise metadata-only primitive views and normalized structure-of-arrays
-- pair views, including truncating zip and nested pairs.
unboxedSlicesAndPairs = do
    putStrLn "unboxed slices and pairs"
    let source = U.fromList [0..7] :: U.Vector Int
    print (U.toList (U.slice 0 8 source))
    print (U.toList (U.slice 2 4 source))
    print (U.toList (U.slice 1 2 (U.slice 2 5 source)))
    print (U.toList (U.unsafeSlice 3 2 source))
    print (U.toList (U.take (-2) source), U.toList (U.take 20 source))
    print (U.toList (U.drop (-2) source), U.toList (U.drop 20 source))
    let pairs = U.zip (U.fromList [1,2,3,4] :: U.Vector Int)
                      (U.fromList [1.5,2.5] :: U.Vector Double)
        (left, right) = U.unzip pairs
    print (U.length pairs, U.toList pairs)
    print (U.length left, U.toList left, U.length right, U.toList right)
    let nested = U.zip pairs (U.fromList [10,20,30] :: U.Vector Int)
    print (U.toList nested)

-- Run the related boxed and unboxed vector checks in one compiler process.
main = do
    boxedConstruction
    boxedGeneration
    boxedInstances
    boxedLazyElements
    boxedLoopLaziness
    boxedSlicing
    unboxedBasic
    unboxedShapeLaziness
    unboxedSlicesAndPairs
