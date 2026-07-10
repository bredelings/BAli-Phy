module Numeric.LinearAlgebra.Data where

import Foreign.CList (mapFrom)

type R = Double
type I = Int

-- Dense native vectors and matrices have nominal element roles because their
-- runtime storage representation is selected by Element.
type role Vector nominal
data Vector a

type role Matrix nominal
data Matrix a

foreign import ecall "Matrix:" rows :: Matrix a -> Int
foreign import ecall "Matrix:" cols :: Matrix a -> Int

foreign import ecall "Prelude:show" showMatrix :: Matrix a -> CPPString
foreign import ecall "Prelude:show" showNumericVector :: Vector a -> CPPString

infixl 4 ><
infixl 4 |>

-- Element selects the native representation used by numeric vectors and
-- matrices.  Instances are limited to representations supported by C++.
class Element a where
    fromList :: [a] -> Vector a
    (|>) :: Int -> [a] -> Vector a
    (><) :: Int -> Int -> [a] -> Matrix a

foreign import bpcall "Matrix:intVectorFromList" intVectorFromList :: [Int] -> Vector Int
foreign import bpcall "Matrix:doubleVectorFromList" doubleVectorFromList :: [Double] -> Vector Double
foreign import bpcall "Matrix:sizedIntVectorFromList" sizedIntVectorFromList :: Int -> [Int] -> Vector Int
foreign import bpcall "Matrix:sizedDoubleVectorFromList" sizedDoubleVectorFromList :: Int -> [Double] -> Vector Double
foreign import bpcall "Matrix:intMatrixFromList" intMatrixFromList :: Int -> Int -> [Int] -> Matrix Int
foreign import bpcall "Matrix:doubleMatrixFromList" doubleMatrixFromList :: Int -> Int -> [Double] -> Matrix Double

instance Element Int where
    fromList = intVectorFromList
    (|>) = sizedIntVectorFromList
    (><) = intMatrixFromList

instance Element Double where
    fromList = doubleVectorFromList
    (|>) = sizedDoubleVectorFromList
    (><) = doubleMatrixFromList

vector :: [Double] -> Vector Double
vector = fromList

range :: Int -> Vector Int
range n = fromList [0..n-1]

idxs :: [Int] -> Vector Int
idxs = fromList

type IndexOf :: (Type -> Type) -> Type
type family IndexOf c
type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int, Int)

-- Container supplies the common shape, indexing, constant, and scalar
-- operations needed immediately by both supported dense container types.
class Element e => Container c e where
    size :: c e -> IndexOf c
    atIndex :: c e -> IndexOf c -> e
    konst :: e -> IndexOf c -> c e
    scalar :: e -> c e
    sumElements :: c e -> e

foreign import ecall "Matrix:" vectorSize :: Vector a -> Int
foreign import ecall "Matrix:" vectorAtIndex :: Vector a -> Int -> a
foreign import ecall "Matrix:" vectorEqual :: Vector a -> Vector a -> Bool
foreign import ecall "Matrix:" vectorSumElements :: Vector a -> a

instance Element a => Container Vector a where
    size = vectorSize
    atIndex = vectorAtIndex
    konst value count = fromList (replicate count value)
    scalar value = fromList [value]
    sumElements = vectorSumElements

foreign import ecall "Matrix:" matrixAtIndex :: Int -> Int -> Matrix a -> a
foreign import ecall "Matrix:" matrixSumElements :: Matrix a -> a

instance Element a => Container Matrix a where
    size matrix = (rows matrix, cols matrix)
    atIndex matrix (i,j) = matrixAtIndex i j matrix
    konst value (rows,columns) = (rows >< columns) (replicate (rows * columns) value)
    scalar value = (1 >< 1) [value]
    sumElements = matrixSumElements

-- Convert a native vector to a lazy Haskell list without constructing an
-- intermediate boxed runtime vector.
toList :: Element a => Vector a -> [a]
toList values = mapFrom 0 (vectorSize values) (vectorAtIndex values)

-- Construct a rectangular matrix from rows, expanding singleton rows to the
-- longest row and rejecting all other incompatible row lengths.
fromLists :: Element a => [[a]] -> Matrix a
fromLists [] = (0 >< 0) []
fromLists xss
    | all compatible lengths = (length xss >< columns) (concatMap expand xss)
    | otherwise = error "Numeric.LinearAlgebra.fromLists: rows have incompatible lengths"
  where
    lengths = map length xss
    columns = maximum lengths
    compatible n = n == columns || n == 1
    expand [x] = replicate columns x
    expand xs = xs

ident :: (Element a, Num a) => Int -> Matrix a
ident dimension = (dimension >< dimension)
                     [if i == j then 1 else 0 | i <- [0..dimension-1], j <- [0..dimension-1]]

foreign import bpcall "Matrix:" matrixToVector :: Matrix a -> Vector a

flatten :: Element a => Matrix a -> Vector a
flatten = matrixToVector

-- Split a row-major list into the requested number of rows, including empty
-- rows when the matrix has zero columns.
splitRows :: Int -> Int -> [a] -> [[a]]
splitRows 0 _ _ = []
splitRows rows columns xs = take columns xs : splitRows (rows-1) columns (drop columns xs)

toLists :: Element a => Matrix a -> [[a]]
toLists matrix = splitRows (rows matrix) (cols matrix) (toList (flatten matrix))

foreign import bpcall "Matrix:" reshapeVector :: Int -> Vector a -> Matrix a
foreign import bpcall "Matrix:" vectorAsRow :: Vector a -> Matrix a
foreign import bpcall "Matrix:" vectorAsColumn :: Vector a -> Matrix a

reshape :: Element a => Int -> Vector a -> Matrix a
reshape = reshapeVector

asRow :: Element a => Vector a -> Matrix a
asRow = vectorAsRow

asColumn :: Element a => Vector a -> Matrix a
asColumn = vectorAsColumn

foreign import bpcall "Matrix:tr" transposeNative :: Matrix a -> Matrix a
foreign import bpcall "Matrix:scale" scaleNative :: a -> Matrix a -> Matrix a

tr :: Element a => Matrix a -> Matrix a
tr = transposeNative

scale :: Element a => a -> Matrix a -> Matrix a
scale = scaleNative

(%*%) = elementwise_multiply

foreign import bpcall "Matrix:" elementwise_multiply :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_add :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_sub :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_mult :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_abs :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_negate :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_signum :: Matrix a -> Matrix a
