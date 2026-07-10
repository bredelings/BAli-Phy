module Numeric.LinearAlgebra.Data where

import Foreign.CList (mapFrom)
import Data.OldList (sort)

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
class (Num a, Ord a) => Element a where
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
    cmap function values = fromList (map function (toList values))
    prodElements = product . toList
    minElement = minimum . toList
    maxElement = maximum . toList
    -- Return the first vector position attaining each extremum.
    minIndex values = let target = minElement values
                      in head [i | i <- [0..size values-1], atIndex values i == target]
    -- Return the first vector position attaining the maximum.
    maxIndex values = let target = maxElement values
                      in head [i | i <- [0..size values-1], atIndex values i == target]
    find predicate values = [i | i <- [0..size values-1], predicate (atIndex values i)]

foreign import ecall "Matrix:" matrixAtIndex :: Int -> Int -> Matrix a -> a
foreign import ecall "Matrix:" matrixSumElements :: Matrix a -> a

instance Element a => Container Matrix a where
    size matrix = (rows matrix, cols matrix)
    atIndex matrix (i,j) = matrixAtIndex i j matrix
    konst value (rows,columns) = (rows >< columns) (replicate (rows * columns) value)
    scalar value = (1 >< 1) [value]
    sumElements = matrixSumElements
    cmap function matrix = (rows matrix >< cols matrix)
        (map function (toList (flatten matrix)))
    prodElements = product . toList . flatten
    minElement = minimum . toList . flatten
    maxElement = maximum . toList . flatten
    -- Return the first row-major matrix position attaining each extremum.
    minIndex matrix = let target = minElement matrix
                      in head [(i,j) | i <- [0..rows matrix-1], j <- [0..cols matrix-1],
                                      atIndex matrix (i,j) == target]
    -- Return the first row-major matrix position attaining the maximum.
    maxIndex matrix = let target = maxElement matrix
                      in head [(i,j) | i <- [0..rows matrix-1], j <- [0..cols matrix-1],
                                      atIndex matrix (i,j) == target]
    find predicate matrix =
        [(i,j) | i <- [0..rows matrix-1], j <- [0..cols matrix-1],
                 predicate (atIndex matrix (i,j))]

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

foreign import bpcall "Matrix:" subVectorNative :: Int -> Int -> Vector a -> Vector a
foreign import bpcall "Matrix:" appendVectorsNative :: Vector a -> Vector a -> Vector a

subVector :: Element a => Int -> Int -> Vector a -> Vector a
subVector = subVectorNative

-- Extract consecutive vector segments, leaving any unrequested suffix unused.
takesV :: Element a => [Int] -> Vector a -> [Vector a]
takesV [] _ = []
takesV (count:counts) values =
    subVector 0 count values :
        takesV counts (subVector count (size values - count) values)

vjoin :: Element a => [Vector a] -> Vector a
vjoin [] = fromList []
vjoin (value:values) = appendVectorsNative value (vjoin values)

foreign import bpcall "Matrix:" subMatrixNative :: Int -> Int -> Int -> Int -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" gatherMatrixNative :: Vector Int -> Vector Int -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" joinMatricesNative :: Int -> Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" repmatNative :: Matrix a -> Int -> Int -> Matrix a

subMatrix :: Element a => (Int, Int) -> (Int, Int) -> Matrix a -> Matrix a
subMatrix (firstRow, firstColumn) (rowCount, columnCount) =
    subMatrixNative firstRow firstColumn rowCount columnCount

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
    gatherMatrixNative (indices (rows matrix) rowExtractor)
                       (indices (cols matrix) columnExtractor) matrix
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
        | otherwise = fromList [position `mod` dimension | position <- toList positions]
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
(|||) = joinMatricesNative 1

(===) :: Element a => Matrix a -> Matrix a -> Matrix a
(===) = joinMatricesNative 0

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
repmat = repmatNative

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

sortVector :: (Element a, Ord a) => Vector a -> Vector a
sortVector = fromList . sort . toList

sortIndex :: (Element a, Ord a) => Vector a -> Vector Int
sortIndex values = fromList (map snd (sort (zip (toList values) [0..])))

conj :: Container c a => c a -> c a
conj = id

cmod :: (Container c a, Integral a) => a -> c a -> c a
cmod divisor = cmap (\value -> value `mod` divisor)

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
