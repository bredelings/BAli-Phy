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

matrix :: Int -> [Double] -> Matrix Double
matrix columns = reshape columns . fromList

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

foreign import bpcall "Matrix:" matrixToVector :: Matrix a -> Vector a

flatten :: Element a => Matrix a -> Vector a
flatten = matrixToVector

toLists :: Element a => Matrix a -> [[a]]
toLists = map toList . toRows

foreign import bpcall "Matrix:" reshapeVector :: Int -> Vector a -> Matrix a
foreign import bpcall "Matrix:" vectorAsRow :: Vector a -> Matrix a
foreign import bpcall "Matrix:" vectorAsColumn :: Vector a -> Matrix a

reshape :: Element a => Int -> Vector a -> Matrix a
reshape = reshapeVector

asRow :: Element a => Vector a -> Matrix a
asRow = vectorAsRow

asColumn :: Element a => Vector a -> Matrix a
asColumn = vectorAsColumn

row :: [Double] -> Matrix Double
row = asRow . fromList

col :: [Double] -> Matrix Double
col = asColumn . fromList

-- Construct rows with hmatrix singleton expansion, rejecting vectors with
-- incompatible non-singleton extents.
fromRows :: Element a => [Vector a] -> Matrix a
fromRows [] = (0 >< 0) []
fromRows vectors =
    case conform (map size vectors) of
        Nothing -> error "Numeric.LinearAlgebra.fromRows: vectors have incompatible sizes"
        Just columns -> (length vectors >< columns) (concatMap (expand columns) vectors)
  where
    -- Find the common extent while retaining zero as the extent of empty rows.
    conform [] = Nothing
    conform [n] = Just n
    conform (n:m:ns)
        | n == m = conform (m:ns)
        | n == 1 = conform (m:ns)
        | m == 1 = conform (n:ns)
        | otherwise = Nothing

    -- Expand a singleton row to the common extent, or preserve a full row.
    expand columns values
        | columns == 0 = []
        | size values == columns = toList values
        | otherwise = replicate columns (atIndex values 0)

-- Extract matrix rows as independent native vectors without passing through
-- a structural EVector.
toRows :: Element a => Matrix a -> [Vector a]
toRows matrix =
    mapFrom 0 (rows matrix) $ \i ->
        fromList (mapFrom 0 (cols matrix) $ \j -> atIndex matrix (i,j))

fromColumns :: Element a => [Vector a] -> Matrix a
fromColumns = tr . fromRows

toColumns :: Element a => Matrix a -> [Vector a]
toColumns = toRows . tr

fromLists :: Element a => [[a]] -> Matrix a
fromLists = fromRows . map fromList

build :: (Element a, Num a) => (Int, Int) -> (a -> a -> a) -> Matrix a
build (rowCount, columnCount) element = (rowCount >< columnCount)
    [element (fromIntegral i) (fromIntegral j)
        | i <- [0..rowCount-1], j <- [0..columnCount-1]]

ident :: (Element a, Num a) => Int -> Matrix a
ident dimension = (dimension >< dimension)
    [if i == j then 1 else 0
        | i <- [0..dimension-1], j <- [0..dimension-1]]

-- Fill a rectangular matrix and replace as much of its main diagonal as the
-- supplied vector and requested dimensions permit.
diagRect :: Element a => a -> Vector a -> Int -> Int -> Matrix a
diagRect fill diagonal rowCount columnCount = (rowCount >< columnCount)
    [if i == j && i < size diagonal then atIndex diagonal i else fill
        | i <- [0..rowCount-1], j <- [0..columnCount-1]]

diag :: (Element a, Num a) => Vector a -> Matrix a
diag diagonal = diagRect 0 diagonal dimension dimension
  where dimension = size diagonal

diagl :: [Double] -> Matrix Double
diagl = diag . fromList

takeDiag :: Element a => Matrix a -> Vector a
takeDiag matrix = fromList
    [atIndex matrix (i,i) | i <- [0..min (rows matrix) (cols matrix)-1]]

-- Construct evenly spaced endpoints, using their midpoint for the
-- single-element case as hmatrix does.
linspace :: (Element a, Fractional a) => Int -> (a, a) -> Vector a
linspace 0 _ = fromList []
linspace 1 (start, end) = fromList [(start + end) / 2]
linspace count (start, end) = fromList
    [start + fromIntegral i * step | i <- [0..count-1]]
  where step = (end - start) / fromIntegral (count - 1)

foreign import bpcall "Matrix:tr" transposeNative :: Matrix a -> Matrix a
foreign import bpcall "Matrix:scale" scaleNative :: a -> Matrix a -> Matrix a

tr :: Element a => Matrix a -> Matrix a
tr = transposeNative

scale :: Element a => a -> Matrix a -> Matrix a
scale = scaleNative

foreign import bpcall "Matrix:" vector_elementwise_multiply :: Vector a -> Vector a -> Vector a
foreign import bpcall "Matrix:" vector_elementwise_add :: Vector a -> Vector a -> Vector a
foreign import bpcall "Matrix:" vector_elementwise_sub :: Vector a -> Vector a -> Vector a
foreign import bpcall "Matrix:" vector_abs :: Vector a -> Vector a
foreign import bpcall "Matrix:" vector_negate :: Vector a -> Vector a
foreign import bpcall "Matrix:" vector_signum :: Vector a -> Vector a
foreign import ecall "Matrix:" dotNative :: Vector a -> Vector a -> a
foreign import bpcall "Matrix:" matrixVectorNative :: Matrix a -> Vector a -> Vector a
foreign import bpcall "Matrix:" vectorMatrixNative :: Vector a -> Matrix a -> Vector a
foreign import bpcall "Matrix:" outerNative :: Vector a -> Vector a -> Matrix a
foreign import bpcall "Matrix:" elementwise_multiply :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_add :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_sub :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_mult :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_abs :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_negate :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_signum :: Matrix a -> Matrix a

-- Scale singleton matrices and otherwise dispatch a conformable product to
-- the native Eigen implementation.
matrixProduct :: Element a => Matrix a -> Matrix a -> Matrix a
matrixProduct left right
    | rows left == 1 && cols left == 1 = scale (atIndex left (0,0)) right
    | rows right == 1 && cols right == 1 = scale (atIndex right (0,0)) left
    | otherwise = mat_mult left right
