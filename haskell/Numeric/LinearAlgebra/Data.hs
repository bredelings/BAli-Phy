{-# LANGUAGE TypeFamilies #-}
module Numeric.LinearAlgebra.Data
    ( R, I
    , NativeVector, NativeMatrix, Vector, Matrix
    , vectorFromNative, matrixFromNative, nativeVector, nativeMatrix
    , vectorSize, rows, cols
    , showMatrixNative, showNumericVectorNative
    , Element(..), IndexOf, Container(..)
    , vector, matrix, range, idxs, toList
    , flatten, toLists, reshape, asRow, asColumn, row, col
    , fromRows, toRows, fromColumns, toColumns, fromLists
    , build, ident, diagRect, diag, diagl, takeDiag, linspace
    , subVector, takesV, vjoin, subMatrix
    , takeRows, dropRows, takeColumns, dropColumns
    , Extractor(..), (??), flipud, fliprl, (|||), (===)
    , fromBlocks, diagBlock, repmat, toBlocks, toBlocksEvery
    , sortVector, sortIndex, conj, cmod, tr, scale
    , vectorEqual, vector_elementwise_multiply, vector_elementwise_add
    , vector_elementwise_sub, vector_abs, vector_negate, vector_signum
    , unaryVector, binaryVector, dotVector
    , matrixVectorProduct, vectorMatrixProduct, outerProduct
    , elementwise_multiply, elementwise_add, elementwise_sub
    , mat_abs, mat_negate, mat_signum, unaryMatrix, binaryMatrix
    , matrixProduct
    ) where

import Compiler.FFI.Runtime (RuntimeValue)
import Compiler.FFI.Import (CInput(..))
import Foreign.CList (mapFrom)
import Foreign.NativeVector (NativeVector)

type R = Double
type I = Int

type role NativeMatrix nominal
data NativeMatrix a

instance RuntimeValue (NativeMatrix a)

-- Keep stable dimensions separate from changing native vector contents.
type role Vector nominal
data Vector a = Vector !Int (NativeVector a)

-- Keep stable dimensions separate from changing native matrix contents.
type role Matrix nominal
data Matrix a = Matrix !Int !Int (NativeMatrix a)

vectorFromNative :: Int -> NativeVector a -> Vector a
vectorFromNative = Vector

matrixFromNative :: Int -> Int -> NativeMatrix a -> Matrix a
matrixFromNative = Matrix

vectorSize :: Vector a -> Int
vectorSize (Vector count _) = count

rows :: Matrix a -> Int
rows (Matrix rowCount _ _) = rowCount

cols :: Matrix a -> Int
cols (Matrix _ columnCount _) = columnCount

nativeVector :: Vector a -> NativeVector a
nativeVector (Vector _ payload) = payload

nativeMatrix :: Matrix a -> NativeMatrix a
nativeMatrix (Matrix _ _ payload) = payload

instance CInput (Matrix a) where
    type CInputType (Matrix a) result = NativeMatrix a -> result
    withCInput value continuation = continuation (nativeMatrix value)

foreign import ecall "Prelude:show" showMatrixNative :: NativeMatrix a -> CPPString
foreign import ecall "Prelude:show" showNumericVectorNative :: NativeVector a -> CPPString

infixl 4 ><
infixl 4 |>

-- Element selects the native representation used by numeric vectors and
-- matrices.  Instances are limited to representations supported by C++.
class (Num a, Ord a) => Element a where
    fromList :: [a] -> Vector a
    (|>) :: Int -> [a] -> Vector a
    (><) :: Int -> Int -> [a] -> Matrix a

foreign import bpcall "NativeVector:sizedIntVectorFromList" sizedIntVectorFromListNative :: Int -> [Int] -> NativeVector Int
foreign import bpcall "NativeVector:sizedDoubleVectorFromList" sizedDoubleVectorFromListNative :: Int -> [Double] -> NativeVector Double
foreign import bpcall "Matrix:intMatrixFromList" intMatrixFromListNative :: Int -> Int -> [Int] -> NativeMatrix Int
foreign import bpcall "Matrix:doubleMatrixFromList" doubleMatrixFromListNative :: Int -> Int -> [Double] -> NativeMatrix Double

instance Element Int where
    fromList values = Vector count (sizedIntVectorFromListNative count values)
      where
        count = length values
    count |> values = Vector count (sizedIntVectorFromListNative count values)
    rowCount >< columnCount = Matrix rowCount columnCount .
        intMatrixFromListNative rowCount columnCount

instance Element Double where
    fromList values = Vector count (sizedDoubleVectorFromListNative count values)
      where
        count = length values
    count |> values = Vector count (sizedDoubleVectorFromListNative count values)
    rowCount >< columnCount = Matrix rowCount columnCount .
        doubleMatrixFromListNative rowCount columnCount

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

-- Container supplies common shape, construction, traversal, and reduction
-- operations for both supported dense container types.
class Element e => Container c e where
    size :: c e -> IndexOf c
    atIndex :: c e -> IndexOf c -> e
    konst :: e -> IndexOf c -> c e
    scalar :: e -> c e
    sumElements :: c e -> e
    cmap :: Element b => (e -> b) -> c e -> c b
    prodElements :: c e -> e
    minElement :: c e -> e
    maxElement :: c e -> e
    minIndex :: c e -> IndexOf c
    maxIndex :: c e -> IndexOf c
    find :: (e -> Bool) -> c e -> [IndexOf c]

foreign import ecall "NativeVector:vectorAtIndex" vectorAtIndexNative :: NativeVector a -> Int -> a
foreign import ecall "Matrix:vectorEqual" vectorEqualNative :: NativeVector a -> NativeVector a -> Bool
foreign import ecall "Matrix:vectorSumElements" vectorSumElementsNative :: NativeVector a -> a
foreign import ecall "Matrix:vectorProductElements" vectorProductElementsNative :: NativeVector a -> a
foreign import ecall "Matrix:vectorMinElement" vectorMinElementNative :: NativeVector a -> a
foreign import ecall "Matrix:vectorMaxElement" vectorMaxElementNative :: NativeVector a -> a
foreign import ecall "Matrix:vectorMinIndex" vectorMinIndexNative :: NativeVector a -> Int
foreign import ecall "Matrix:vectorMaxIndex" vectorMaxIndexNative :: NativeVector a -> Int
foreign import bpcall "NativeVector:vectorKonstNative" vectorKonstNative :: a -> Int -> NativeVector a
foreign import bpcall "Matrix:matrixKonstNative" matrixKonstNative :: a -> Int -> Int -> NativeMatrix a

vectorEqual :: Vector a -> Vector a -> Bool
vectorEqual left right = vectorEqualNative (nativeVector left) (nativeVector right)

instance Element a => Container Vector a where
    size = vectorSize
    atIndex values index = vectorAtIndexNative (nativeVector values) index
    konst value count = Vector count (vectorKonstNative value count)
    scalar value = Vector 1 (vectorKonstNative value 1)
    sumElements = vectorSumElementsNative . nativeVector
    cmap function values = fromList (map function (toList values))
    prodElements = vectorProductElementsNative . nativeVector
    minElement = vectorMinElementNative . nativeVector
    maxElement = vectorMaxElementNative . nativeVector
    minIndex = vectorMinIndexNative . nativeVector
    maxIndex = vectorMaxIndexNative . nativeVector
    find predicate values = [i | i <- [0..size values-1], predicate (atIndex values i)]

foreign import ecall "Matrix:matrixAtIndex" matrixAtIndexNative :: Int -> Int -> NativeMatrix a -> a
foreign import ecall "Matrix:matrixSumElements" matrixSumElementsNative :: NativeMatrix a -> a
foreign import ecall "Matrix:matrixProductElements" matrixProductElementsNative :: NativeMatrix a -> a
foreign import ecall "Matrix:matrixMinElement" matrixMinElementNative :: NativeMatrix a -> a
foreign import ecall "Matrix:matrixMaxElement" matrixMaxElementNative :: NativeMatrix a -> a
foreign import ecall "Matrix:matrixMinIndex" matrixMinIndexNative :: NativeMatrix a -> Int
foreign import ecall "Matrix:matrixMaxIndex" matrixMaxIndexNative :: NativeMatrix a -> Int

instance Element a => Container Matrix a where
    size matrix = (rows matrix, cols matrix)
    atIndex matrix (i,j) = matrixAtIndexNative i j (nativeMatrix matrix)
    konst value (rows,columns) = Matrix rows columns
        (matrixKonstNative value rows columns)
    scalar value = Matrix 1 1 (matrixKonstNative value 1 1)
    sumElements = matrixSumElementsNative . nativeMatrix
    cmap function matrix = (rows matrix >< cols matrix)
        (map function (toList (flatten matrix)))
    prodElements = matrixProductElementsNative . nativeMatrix
    minElement = matrixMinElementNative . nativeMatrix
    maxElement = matrixMaxElementNative . nativeMatrix
    minIndex matrix = matrixMinIndexNative (nativeMatrix matrix) `divMod` cols matrix
    maxIndex matrix = matrixMaxIndexNative (nativeMatrix matrix) `divMod` cols matrix
    find predicate matrix =
        [(i,j) | i <- [0..rows matrix-1], j <- [0..cols matrix-1],
                 predicate (atIndex matrix (i,j))]

-- Convert a native vector to a lazy Haskell list without constructing an
-- intermediate boxed runtime vector.
toList :: Element a => Vector a -> [a]
toList values = mapFrom 0 (vectorSize values) $ \index ->
    vectorAtIndexNative (nativeVector values) index

foreign import bpcall "Matrix:matrixToVector" matrixToVectorNative :: NativeMatrix a -> NativeVector a

flatten :: Element a => Matrix a -> Vector a
flatten matrix = Vector (rows matrix * cols matrix)
    (matrixToVectorNative (nativeMatrix matrix))

toLists :: Element a => Matrix a -> [[a]]
toLists matrix = mapFrom 0 (rows matrix) $ \i ->
    mapFrom 0 (cols matrix) $ \j -> atIndex matrix (i,j)

foreign import bpcall "Matrix:reshapeVector" reshapeVectorNative :: Int -> NativeVector a -> NativeMatrix a
foreign import bpcall "Matrix:vectorAsRow" vectorAsRowNative :: NativeVector a -> NativeMatrix a
foreign import bpcall "Matrix:vectorAsColumn" vectorAsColumnNative :: NativeVector a -> NativeMatrix a
foreign import bpcall "Matrix:matrixFromRowsNative" matrixFromRowsNative :: Int -> Int -> [NativeVector a] -> NativeMatrix a
foreign import bpcall "Matrix:matrixFromColumnsNative" matrixFromColumnsNative :: Int -> Int -> [NativeVector a] -> NativeMatrix a
foreign import bpcall "Matrix:matrixRowNative" matrixRowNative :: Int -> NativeMatrix a -> NativeVector a
foreign import bpcall "Matrix:matrixColumnNative" matrixColumnNative :: Int -> NativeMatrix a -> NativeVector a

-- Reshape a vector while deriving the matrix dimensions without inspecting
-- the native payload.
reshape :: Element a => Int -> Vector a -> Matrix a
reshape columnCount values = Matrix rowCount columnCount
    (reshapeVectorNative columnCount (nativeVector values))
  where rowCount = if columnCount == 0 then 0 else vectorSize values `div` columnCount

asRow :: Element a => Vector a -> Matrix a
asRow values = Matrix 1 (vectorSize values)
    (vectorAsRowNative (nativeVector values))

asColumn :: Element a => Vector a -> Matrix a
asColumn values = Matrix (vectorSize values) 1
    (vectorAsColumnNative (nativeVector values))

row :: [Double] -> Matrix Double
row = asRow . fromList

col :: [Double] -> Matrix Double
col = asColumn . fromList

-- Find the shared row extent, allowing singleton rows to expand to the
-- largest compatible extent.
conformRows :: [Int] -> Maybe Int
conformRows [] = Nothing
conformRows [n] = Just n
conformRows (n:m:ns)
    | n == m = conformRows (m:ns)
    | n == 1 = conformRows (m:ns)
    | m == 1 = conformRows (n:ns)
    | otherwise = Nothing

-- Construct rows with hmatrix singleton expansion, rejecting vectors with
-- incompatible non-singleton extents.
fromRows :: Element a => [Vector a] -> Matrix a
fromRows [] = (0 >< 0) []
fromRows vectors =
    case conformRows (map size vectors) of
        Nothing -> error "Numeric.LinearAlgebra.fromRows: vectors have incompatible sizes"
        Just 0 -> (length vectors >< 0) []
        Just columns -> Matrix (length vectors) columns
            (matrixFromRowsNative (length vectors) columns (map nativeVector vectors))

-- Extract matrix rows as independent native vectors without passing through
-- a structural EVector.
toRows :: Element a => Matrix a -> [Vector a]
toRows matrix =
    mapFrom 0 (rows matrix) $ \i ->
        Vector (cols matrix) (matrixRowNative i (nativeMatrix matrix))

fromColumns :: Element a => [Vector a] -> Matrix a
fromColumns [] = (0 >< 0) []
fromColumns vectors =
    case conformRows (map size vectors) of
        Nothing -> error "Numeric.LinearAlgebra.fromRows: vectors have incompatible sizes"
        Just 0 -> (0 >< length vectors) []
        Just rowCount -> Matrix rowCount (length vectors)
            (matrixFromColumnsNative (length vectors) rowCount (map nativeVector vectors))

toColumns :: Element a => Matrix a -> [Vector a]
toColumns matrix =
    mapFrom 0 (cols matrix) $ \j ->
        Vector (rows matrix) (matrixColumnNative j (nativeMatrix matrix))

-- Construct directly from scalar rows without creating temporary native
-- vectors that would immediately be converted back into lists.
fromLists :: Element a => [[a]] -> Matrix a
fromLists [] = (0 >< 0) []
fromLists rowLists =
    case conformRows (map length rowLists) of
        Nothing -> error "Numeric.LinearAlgebra.fromRows: vectors have incompatible sizes"
        Just columns -> (length rowLists >< columns)
            (concatMap (expand columns) rowLists)
  where
    expand columns values
        | columns == 0 = []
        | length values == columns = values
        | otherwise = replicate columns (head values)

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
foreign import bpcall "Matrix:diagRectNative" diagRectNative :: a -> NativeVector a -> Int -> Int -> NativeMatrix a
foreign import bpcall "Matrix:takeDiagNative" takeDiagNative :: NativeMatrix a -> NativeVector a

diagRect :: Element a => a -> Vector a -> Int -> Int -> Matrix a
diagRect fill diagonal rowCount columnCount = Matrix rowCount columnCount
    (diagRectNative fill (nativeVector diagonal) rowCount columnCount)

diag :: (Element a, Num a) => Vector a -> Matrix a
diag diagonal = diagRect 0 diagonal dimension dimension
  where dimension = size diagonal

diagl :: [Double] -> Matrix Double
diagl = diag . fromList

takeDiag :: Element a => Matrix a -> Vector a
takeDiag matrix = Vector count (takeDiagNative (nativeMatrix matrix))
  where count = min (rows matrix) (cols matrix)

-- Construct evenly spaced endpoints, using their midpoint for the
-- single-element case as hmatrix does.
linspace :: (Element a, Fractional a) => Int -> (a, a) -> Vector a
linspace 0 _ = fromList []
linspace 1 (start, end) = fromList [(start + end) / 2]
linspace count (start, end) = fromList
    [start + fromIntegral i * step | i <- [0..count-1]]
  where step = (end - start) / fromIntegral (count - 1)

foreign import bpcall "Matrix:subVectorNative" subVectorNative :: Int -> Int -> NativeVector a -> NativeVector a
foreign import bpcall "Matrix:joinVectorsNative" joinVectorsNative :: Int -> [NativeVector a] -> NativeVector a

subVector :: Element a => Int -> Int -> Vector a -> Vector a
subVector start count values = Vector count
    (subVectorNative start count (nativeVector values))

-- Extract consecutive vector segments, leaving any unrequested suffix unused.
takesV :: Element a => [Int] -> Vector a -> [Vector a]
takesV counts values = segments 0 counts
  where
    segments _ [] = []
    segments offset (count:remaining) =
        subVector offset count values : segments (offset + count) remaining

-- Join vectors while recording their combined length independently of the
-- appended native payload.
vjoin :: Element a => [Vector a] -> Vector a
vjoin [] = fromList []
vjoin values = Vector total
    (joinVectorsNative total (map nativeVector values))
  where total = sum (map vectorSize values)

foreign import bpcall "Matrix:subMatrixNative" subMatrixNative :: Int -> Int -> Int -> Int -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:gatherMatrixNative" gatherMatrixNative :: NativeVector Int -> NativeVector Int -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:joinMatricesNative" joinMatricesNative :: Int -> NativeMatrix a -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:repmatNative" repmatNative :: NativeMatrix a -> Int -> Int -> NativeMatrix a
foreign import bpcall "Matrix:vectorModuloNative" vectorModuloNative :: NativeVector Int -> Int -> NativeVector Int

-- Extract a rectangular payload and record its requested shape directly.
subMatrix :: Element a => (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
subMatrix (firstRow, firstColumn) (rowCount, columnCount) matrix =
    Matrix rowCount columnCount
        (subMatrixNative firstRow firstColumn rowCount columnCount
            (nativeMatrix matrix))

takeRows :: Element a => Int -> Matrix a -> Matrix a
takeRows count matrix = subMatrix (0,0) (count, cols matrix) matrix

dropRows :: Element a => Int -> Matrix a -> Matrix a
dropRows count matrix = subMatrix (count,0) (rows matrix-count, cols matrix) matrix

takeColumns :: Element a => Int -> Matrix a -> Matrix a
takeColumns count matrix = subMatrix (0,0) (rows matrix, count) matrix

dropColumns :: Element a => Int -> Matrix a -> Matrix a
dropColumns count matrix = subMatrix (0,count) (rows matrix, cols matrix-count) matrix

data Extractor = All
               | Range Int Int Int
               | Pos (Vector Int)
               | PosCyc (Vector Int)
               | Take Int
               | TakeLast Int
               | Drop Int
               | DropLast Int

infixl 9 ??

-- Convert each public extraction form to native indices and gather both
-- dimensions in one pass over the source matrix.
(??) :: Element a => Matrix a -> (Extractor, Extractor) -> Matrix a
matrix ?? (rowExtractor, columnExtractor) =
    let rowIndices = indices (rows matrix) rowExtractor
        columnIndices = indices (cols matrix) columnExtractor
    in Matrix (vectorSize rowIndices) (vectorSize columnIndices)
        (gatherMatrixNative (nativeVector rowIndices)
                             (nativeVector columnIndices)
                             (nativeMatrix matrix))
  where
    -- Interpret one extractor against a concrete matrix extent.
    indices dimension All = range dimension
    indices _ (Range start step end)
        | step == 0 = error "Numeric.LinearAlgebra.(??): Range step cannot be zero"
        | otherwise = idxs [start,start+step..end]
    indices _ (Pos positions) = positions
    indices dimension (PosCyc positions)
        | dimension == 0 && size positions /= 0 =
            error "Numeric.LinearAlgebra.(??): cannot cycle indexes into an empty dimension"
        | dimension == 0 = positions
        | otherwise = Vector (vectorSize positions)
            (vectorModuloNative (nativeVector positions) dimension)
    indices dimension (Take count) = idxs [0..min dimension (max 0 count)-1]
    indices dimension (TakeLast count) =
        idxs [max 0 (dimension-max 0 count)..dimension-1]
    indices dimension (Drop count) = idxs [min dimension (max 0 count)..dimension-1]
    indices dimension (DropLast count) =
        idxs [0..max 0 (dimension-max 0 count)-1]

flipud :: Element a => Matrix a -> Matrix a
flipud matrix = matrix ?? (Range (rows matrix-1) (-1) 0, All)

fliprl :: Element a => Matrix a -> Matrix a
fliprl matrix = matrix ?? (All, Range (cols matrix-1) (-1) 0)

infixl 3 |||
infixl 2 ===

(|||) :: Element a => Matrix a -> Matrix a -> Matrix a
left ||| right = Matrix (max (rows left) (rows right))
    (cols left + cols right)
    (joinMatricesNative 1 (nativeMatrix left) (nativeMatrix right))

(===) :: Element a => Matrix a -> Matrix a -> Matrix a
top === bottom = Matrix (rows top + rows bottom)
    (max (cols top) (cols bottom))
    (joinMatricesNative 0 (nativeMatrix top) (nativeMatrix bottom))

-- Assemble block rows through native concatenation so each element remains in
-- Eigen-backed storage throughout singleton expansion and copying.
fromBlocks :: Element a => [[Matrix a]] -> Matrix a
fromBlocks blocks = stack (map joinRow blocks)
  where
    -- Join one horizontal block row from right to left.
    joinRow [] = (0 >< 0) []
    joinRow [matrix] = matrix
    joinRow (matrix:matrices) = matrix ||| joinRow matrices
    -- Stack the completed block rows from top to bottom.
    stack [] = (0 >< 0) []
    stack [matrix] = matrix
    stack (matrix:matrices) = matrix === stack matrices

-- Place matrices on a block diagonal and fill every off-diagonal block with
-- zeros having the corresponding row and column extents.
diagBlock :: (Element a, Num a) => [Matrix a] -> Matrix a
diagBlock matrices = fromBlocks
    [[if i == j then matrix else konst 0 (rows matrix, cols other)
        | (j,other) <- zip [0..] matrices]
        | (i,matrix) <- zip [0..] matrices]

repmat :: Element a => Matrix a -> Int -> Int -> Matrix a
repmat matrix rowRepeats columnRepeats =
    Matrix (rows matrix * rowRepeats) (cols matrix * columnRepeats)
        (repmatNative (nativeMatrix matrix) rowRepeats columnRepeats)

-- Partition the requested prefix into explicit block sizes, discarding any
-- rows or columns not covered by those sizes.
toBlocks :: Element a => [Int] -> [Int] -> Matrix a -> [[Matrix a]]
toBlocks rowSizes columnSizes matrix
    | any (< 0) rowSizes || any (< 0) columnSizes =
        error "Numeric.LinearAlgebra.toBlocks: block sizes must be nonnegative"
    | otherwise = rowsAt 0 rowSizes
  where
    -- Walk the requested row sizes while retaining their source offset.
    rowsAt _ [] = []
    rowsAt firstRow (rowCount:remainingRows) =
        columnsAt firstRow rowCount 0 columnSizes :
            rowsAt (firstRow+rowCount) remainingRows
    -- Walk one block row's column sizes while retaining their source offset.
    columnsAt _ _ _ [] = []
    columnsAt firstRow rowCount firstColumn (columnCount:remainingColumns) =
        subMatrix (firstRow,firstColumn) (rowCount,columnCount) matrix :
            columnsAt firstRow rowCount (firstColumn+columnCount) remainingColumns

-- Cover the complete matrix with equal blocks and smaller final blocks when
-- an extent is not divisible by the requested block size.
toBlocksEvery :: Element a => Int -> Int -> Matrix a -> [[Matrix a]]
toBlocksEvery rowSize columnSize matrix
    | rowSize <= 0 || columnSize <= 0 =
        error "Numeric.LinearAlgebra.toBlocksEvery: block sizes must be positive"
    | otherwise = toBlocks (sizes rowSize (rows matrix))
                           (sizes columnSize (cols matrix)) matrix
  where
    sizes _ 0 = []
    sizes blockSize remaining =
        let current = min blockSize remaining
        in current : sizes blockSize (remaining-current)

foreign import bpcall "Matrix:sortVectorNative" sortVectorNative :: NativeVector a -> NativeVector a
foreign import bpcall "Matrix:sortIndexNative" sortIndexNative :: NativeVector a -> NativeVector Int

sortVector :: (Element a, Ord a) => Vector a -> Vector a
sortVector values = Vector (vectorSize values)
    (sortVectorNative (nativeVector values))

sortIndex :: (Element a, Ord a) => Vector a -> Vector Int
sortIndex values = Vector (vectorSize values)
    (sortIndexNative (nativeVector values))

conj :: Container c a => c a -> c a
conj = id

cmod :: (Container c a, Integral a) => a -> c a -> c a
cmod divisor = cmap (\value -> value `mod` divisor)

foreign import bpcall "Matrix:tr" transposeNative :: NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:scale" scaleNative :: a -> NativeMatrix a -> NativeMatrix a

tr :: Element a => Matrix a -> Matrix a
tr matrix = Matrix (cols matrix) (rows matrix)
    (transposeNative (nativeMatrix matrix))

scale :: Element a => a -> Matrix a -> Matrix a
scale factor matrix = Matrix (rows matrix) (cols matrix)
    (scaleNative factor (nativeMatrix matrix))

foreign import bpcall "Matrix:vector_elementwise_multiply" vectorMultiplyNative :: NativeVector a -> NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vector_elementwise_add" vectorAddNative :: NativeVector a -> NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vector_elementwise_sub" vectorSubtractNative :: NativeVector a -> NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vector_abs" vectorAbsNative :: NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vector_negate" vectorNegateNative :: NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vector_signum" vectorSignumNative :: NativeVector a -> NativeVector a
foreign import ecall "Matrix:dotNative" dotNative :: NativeVector a -> NativeVector a -> a
foreign import bpcall "Matrix:matrixVectorNative" matrixVectorNative :: NativeMatrix a -> NativeVector a -> NativeVector a
foreign import bpcall "Matrix:vectorMatrixNative" vectorMatrixNative :: NativeVector a -> NativeMatrix a -> NativeVector a
foreign import bpcall "Matrix:outerNative" outerNative :: NativeVector a -> NativeVector a -> NativeMatrix a
foreign import bpcall "Matrix:elementwise_multiply" matrixMultiplyElementsNative :: NativeMatrix a -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:elementwise_add" matrixAddNative :: NativeMatrix a -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:elementwise_sub" matrixSubtractNative :: NativeMatrix a -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:mat_mult" matrixProductNative :: NativeMatrix a -> NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:mat_abs" matrixAbsNative :: NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:mat_negate" matrixNegateNative :: NativeMatrix a -> NativeMatrix a
foreign import bpcall "Matrix:mat_signum" matrixSignumNative :: NativeMatrix a -> NativeMatrix a

vector_elementwise_multiply :: Vector a -> Vector a -> Vector a
vector_elementwise_multiply = binaryVector vectorMultiplyNative

vector_elementwise_add :: Vector a -> Vector a -> Vector a
vector_elementwise_add = binaryVector vectorAddNative

vector_elementwise_sub :: Vector a -> Vector a -> Vector a
vector_elementwise_sub = binaryVector vectorSubtractNative

vector_abs :: Vector a -> Vector a
vector_abs = unaryVector vectorAbsNative

vector_negate :: Vector a -> Vector a
vector_negate = unaryVector vectorNegateNative

vector_signum :: Vector a -> Vector a
vector_signum = unaryVector vectorSignumNative

unaryVector :: (NativeVector a -> NativeVector b) -> Vector a -> Vector b
unaryVector operation values = Vector (vectorSize values)
    (operation (nativeVector values))

-- Apply a native binary operation and record its singleton-broadcast extent.
binaryVector :: (NativeVector a -> NativeVector b -> NativeVector c)
             -> Vector a -> Vector b -> Vector c
binaryVector operation left right = Vector (max (vectorSize left) (vectorSize right))
    (operation (nativeVector left) (nativeVector right))

dotVector :: Vector a -> Vector a -> a
dotVector left right = dotNative (nativeVector left) (nativeVector right)

matrixVectorProduct :: Matrix a -> Vector a -> Vector a
matrixVectorProduct matrix vector = Vector (rows matrix)
    (matrixVectorNative (nativeMatrix matrix) (nativeVector vector))

vectorMatrixProduct :: Vector a -> Matrix a -> Vector a
vectorMatrixProduct vector matrix = Vector (cols matrix)
    (vectorMatrixNative (nativeVector vector) (nativeMatrix matrix))

outerProduct :: Vector a -> Vector a -> Matrix a
outerProduct left right = Matrix (vectorSize left) (vectorSize right)
    (outerNative (nativeVector left) (nativeVector right))

elementwise_multiply :: Matrix a -> Matrix a -> Matrix a
elementwise_multiply = binaryMatrix matrixMultiplyElementsNative

elementwise_add :: Matrix a -> Matrix a -> Matrix a
elementwise_add = binaryMatrix matrixAddNative

elementwise_sub :: Matrix a -> Matrix a -> Matrix a
elementwise_sub = binaryMatrix matrixSubtractNative

mat_abs :: Matrix a -> Matrix a
mat_abs = unaryMatrix matrixAbsNative

mat_negate :: Matrix a -> Matrix a
mat_negate = unaryMatrix matrixNegateNative

mat_signum :: Matrix a -> Matrix a
mat_signum = unaryMatrix matrixSignumNative

unaryMatrix :: (NativeMatrix a -> NativeMatrix b) -> Matrix a -> Matrix b
unaryMatrix operation matrix = Matrix (rows matrix) (cols matrix)
    (operation (nativeMatrix matrix))

-- Apply a native binary operation and record both singleton-broadcast extents.
binaryMatrix :: (NativeMatrix a -> NativeMatrix b -> NativeMatrix c)
             -> Matrix a -> Matrix b -> Matrix c
binaryMatrix operation left right =
    Matrix (max (rows left) (rows right)) (max (cols left) (cols right))
        (operation (nativeMatrix left) (nativeMatrix right))

-- Scale singleton matrices and otherwise dispatch a conformable product to
-- the native Eigen implementation.
matrixProduct :: Element a => Matrix a -> Matrix a -> Matrix a
matrixProduct left right
    | rows left == 1 && cols left == 1 = scale (atIndex left (0,0)) right
    | rows right == 1 && cols right == 1 = scale (atIndex right (0,0)) left
    | otherwise = Matrix (rows left) (cols right)
        (matrixProductNative (nativeMatrix left) (nativeMatrix right))
