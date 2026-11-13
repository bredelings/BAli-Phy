{-# LANGUAGE NoImplicitPrelude #-}
import Compiler.Num
import Compiler.Fractional
import Compiler.Error (error)
import Data.Eq
import Data.Bool
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Tuple vectors store separate component arrays, so mixed fields and sliced
-- concatenation catch errors missed by primitive-vector tests. These checks are
-- needed while the arities have separately implemented storage operations.
main = do
    let value3 = (1,2.5,3) :: (Int,Double,Int)
    let other3 = (11,12.5,13)
    let v3 = U.fromList [value3,other3]
    print (U.toList v3 == [value3,other3]
        && U.toList (U.replicate 2 value3) == [value3,value3]
        && U.toList (U.concat [U.slice 1 1 v3, U.slice 0 1 v3]) == [other3,value3]
        && U.length (U.slice 1 0 v3) == 0)
    print (U.length (U.replicate 2 (error "unused" :: (Int,Double,Int))) == 2)
    print (U.toList (U.empty :: U.Vector (Int,Double,Int)) == [])
    let value4 = (1,2.5,3,4.5) :: (Int,Double,Int,Double)
    let other4 = (11,12.5,13,14.5)
    let v4 = U.fromList [value4,other4]
    print (U.toList v4 == [value4,other4]
        && U.toList (U.replicate 2 value4) == [value4,value4]
        && U.toList (U.concat [U.slice 1 1 v4, U.slice 0 1 v4]) == [other4,value4]
        && U.length (U.slice 1 0 v4) == 0)
    print (U.length (U.replicate 2 (error "unused" :: (Int,Double,Int,Double))) == 2)
    print (U.toList (U.empty :: U.Vector (Int,Double,Int,Double)) == [])
    let value5 = (1,2.5,3,4.5,5) :: (Int,Double,Int,Double,Int)
    let other5 = (11,12.5,13,14.5,15)
    let v5 = U.fromList [value5,other5]
    print (U.toList v5 == [value5,other5]
        && U.toList (U.replicate 2 value5) == [value5,value5]
        && U.toList (U.concat [U.slice 1 1 v5, U.slice 0 1 v5]) == [other5,value5]
        && U.length (U.slice 1 0 v5) == 0)
    print (U.length (U.replicate 2 (error "unused" :: (Int,Double,Int,Double,Int))) == 2)
    print (U.toList (U.empty :: U.Vector (Int,Double,Int,Double,Int)) == [])
    let value6 = (1,2.5,3,4.5,5,6.5) :: (Int,Double,Int,Double,Int,Double)
    let other6 = (11,12.5,13,14.5,15,16.5)
    let v6 = U.fromList [value6,other6]
    print (U.toList v6 == [value6,other6]
        && U.toList (U.replicate 2 value6) == [value6,value6]
        && U.toList (U.concat [U.slice 1 1 v6, U.slice 0 1 v6]) == [other6,value6]
        && U.length (U.slice 1 0 v6) == 0)
    print (U.length (U.replicate 2 (error "unused" :: (Int,Double,Int,Double,Int,Double))) == 2)
    print (U.toList (U.empty :: U.Vector (Int,Double,Int,Double,Int,Double)) == [])
