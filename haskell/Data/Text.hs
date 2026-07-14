{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Text
    (Text,
     pack, unpack,
     singleton, empty, cons, snoc, append, uncons,
     head, last, tail, init,
     null, length,
     concat,
     intercalate,
     fromCppString, toCppString,
     doubleToText)
    where

-- A CPPString is an opaque C++ std::string transport value.  Text gives it
-- UTF-8 semantics: Text stores a validated UTF-8 byte string plus a byte
-- offset and byte length that must point to UTF-8 scalar boundaries.  Public
-- character operations count Unicode code points, not graphemes.

import Prelude as P hiding (null, head, last, tail, init, length, empty, intercalate, intersperse, concat, uncons)
import qualified Prelude as P
import Data.Char
import Data.Ord
import Data.Eq
import Data.Semigroup
import Data.Monoid
import Control.DeepSeq
import Data.String

import qualified Foreign.String as FS
import Compiler.FFI.Import (CInput(..), COutput(..))

-- These fields should all be strict.  The Int fields are byte offset and byte
-- length, not code-point offset and code-point length.
data Text = Text CPPString Int Int

foreign import ecall "Text:validate" builtin_validate :: CPPString -> CPPString

foreign import ecall "Text:pack" builtin_pack :: EVector Char -> CPPString

instance Show Text where
    show s = show $ unpack s

pack = fromCppString . builtin_pack . toVector

foreign import ecall "Text:unpack" builtin_unpack :: CPPString -> Int -> Int -> EVector Char
unpack (Text array offset len) = vectorToList $ builtin_unpack array offset len

foreign import ecall "Text:singleton" builtin_singleton :: Char -> CPPString
singleton c = fromCppString $ builtin_singleton c

foreign import ecall "Text:empty" builtin_empty :: CPPString
empty = Text builtin_empty 0 0

infixr 5 `cons`
cons c t = append (singleton c) t

snoc t c = append t (singleton c)

text arr off len | len == 0  = empty
                 | otherwise = Text arr off len

foreign import ecall "Text:append" builtin_append :: CPPString -> Int -> Int ->
                                                      CPPString -> Int -> Int ->
                                                      CPPString
append t1@(Text array1 offset1 len1) t2@(Text array2 offset2 len2)
    | len1 == 0  = t2
    | len2 == 0  = t1
    | otherwise  = Text array3 0 len
    where
      len = len1 + len2
      array3 = builtin_append array1 offset1 len1 array2 offset2 len2

uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)

-- unsnoc :: Text -> Maybe (Text, Char)

foreign import ecall "Text:head" builtin_head :: CPPString -> Int -> Int -> Char
head (Text arr off len)
    | len <= 0  = error "head: empty Text"
    | otherwise = builtin_head arr off len

foreign import ecall "Text:last" builtin_last :: CPPString -> Int -> Int -> Char
last (Text arr off len)
    | len <= 0  = error "last: empty Text"
    | otherwise = builtin_last arr off len

foreign import ecall "Text:tailOffset" builtin_tailOffset :: CPPString -> Int -> Int -> Int
tail (Text arr off len)
    | len <= 0  = error "tail: empty Text"
    | otherwise = text arr off2 (off + len - off2)
    where off2 = builtin_tailOffset arr off len

foreign import ecall "Text:initLength" builtin_initLength :: CPPString -> Int -> Int -> Int
init (Text arr off len)
    | len <= 0 = error "init: empty Text"
    | otherwise = text arr off (builtin_initLength arr off len)

null (Text _ _ len) = len <= 0

foreign import ecall "Text:codePointLength" builtin_codePointLength :: CPPString -> Int -> Int -> Int
length (Text arr off len) = builtin_codePointLength arr off len

-- compareLength :: Text -> Int -> Ordering

-- map :: (Char -> Char) -> Text -> Text

intercalate :: Text -> [Text] -> Text
intercalate i ts = concat $ P.intersperse i ts

-- intersperse :: Char -> Text -> Text

-- transpose :: [Text] -> [Text]

-- reverse :: Text -> Text

-- replace :: Text -> Text -> Text -> Text

-- toCaseFold :: Text -> Text

-- toLower :: Text -> Text

-- toUpper :: Text -> Text

-- toTitle :: Text -> Text

-- justifyLeft :: Int -> Char -> Text -> Text

-- justifyRight :: Int -> Char -> Text -> Text

-- center :: Int -> Char -> Text -> Text

-- foldl :: (a -> Char -> a) -> a -> Text -> a

-- foldl' :: (a -> Char -> a) -> a -> Text -> a

-- foldl1 :: (Char -> Char -> Char) -> Text -> Char

-- foldl1' :: (Char -> Char -> Char) -> Text -> Char

-- foldr :: (Char -> a -> a) -> a -> Text -> a

-- foldr1 :: (Char -> Char -> Char) -> Text -> Char

fromCppString s = Text s' 0 (FS.sizeOfString s') where s' = builtin_validate s

foreign import ecall "Text:" concatRaw :: EVector CPPString -> CPPString

concat :: [Text] -> Text
concat texts = fromCppString $ concatRaw $ toVector $ map toCppString texts

-- concatMap :: (Char -> Text) -> Text -> Text

-- any :: (Char -> Bool) -> Text -> Bool

-- all :: (Char -> Bool) -> Text -> Bool

-- maximum :: Text -> Char

-- minimum :: Text -> Char


-- scanl :: (Char -> Char -> Char) -> Char -> Text -> Text

-- scanl1 :: (Char -> Char -> Char) -> Text -> Text

-- scanr :: (Char -> Char -> Char) -> Char -> Text -> Text

-- scanr1 :: (Char -> Char -> Char) -> Text -> Text


-- mapAccumL :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)

-- mapAccumR :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)

-- replicate :: Int -> Text -> Text

-- unfoldr :: (a -> Maybe (Char, a)) -> a -> Text

-- unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Text

-- take :: Int -> Text -> Text

-- takeEnd :: Int -> Text -> Text

-- drop :: Int -> Text -> Text

-- dropEnd :: Int -> Text -> Text

-- takeWhile :: (Char -> Bool) -> Text -> Text

-- takeWhileEnd :: (Char -> Bool) -> Text -> Text

-- dropWhile :: (Char -> Bool) -> Text -> Text

-- dropWhileEnd :: (Char -> Bool) -> Text -> Text

-- dropAround :: (Char -> Bool) -> Text -> Text

-- strip :: Text -> Text

-- stripStart :: Text -> Text

-- stripEnd :: Text -> Text

-- splitAt :: Int -> Text -> (Text, Text)

-- breakOn :: Text -> Text -> (Text, Text)

-- breakOnEnd :: Text -> Text -> (Text, Text)

-- break :: (Char -> Bool) -> Text -> (Text, Text)

-- span :: (Char -> Bool) -> Text -> (Text, Text)

-- group :: Text -> [Text]

-- groupBy :: (Char -> Char -> Bool) -> Text -> [Text]

-- inits :: Text -> [Text]

-- tails :: Text -> [Text]

-- splitOn :: Text -> Text -> [Text]

-- split :: (Char -> Bool) -> Text -> [Text]

-- chunksOf :: Int -> Text -> [Text]

-- lines :: Text -> [Text]

-- words :: Text -> [Text]

-- unlines :: [Text] -> Text

-- unwords :: [Text] -> Text

-- isPrefixOf :: Text -> Text -> Bool

-- isSuffixOf :: Text -> Text -> Bool

-- isInfixOf :: Text -> Text -> Bool

-- stripPrefix :: Text -> Text -> Maybe Text

-- stripSuffix :: Text -> Text -> Maybe Text

-- commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)

-- filter :: (Char -> Bool) -> Text -> Text

-- breakOnAll :: HasCallStack -> Text -> [(Text,Text)]

-- find :: (Char -> Bool) -> Text -> Maybe Char

-- elem :: Char -> Text -> Bool

-- partition :: (Char -> Bool) -> Text -> (Text, Text)

-- index :: Text -> Int -> Char

-- findIndex :: (Char -> Bool) -> Text -> Maybe Int

-- count :: Text -> Text -> Int

-- zip :: Text -> Text -> [(Char, Char)]

-- zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text

-- copy :: Text -> Text

-- unpackCString# :: Addr# -> Text

-- unpackCStringAscii# :: Addr# -> Text

-- measureOff :: Int -> Text -> Int

foreign import ecall "Text:equals" builtin_equals :: CPPString -> Int -> Int ->
                                                      CPPString -> Int -> Int ->
                                                      Bool

foreign import ecall "Text:less_than" builtin_less_than :: CPPString -> Int -> Int ->
                                                            CPPString -> Int -> Int ->
                                                            Bool

instance Eq Text where
    (Text arr1 off1 len1) == (Text arr2 off2 len2) = builtin_equals arr1 off1 len1 arr2 off2 len2

instance Ord Text where
    (Text arr1 off1 len1) < (Text arr2 off2 len2) = builtin_less_than arr1 off1 len1 arr2 off2 len2

foreign import ecall "Prelude:show_double" doubleToCPPString :: Double -> CPPString
doubleToText d = fromCppString (doubleToCPPString d)


toCppString (Text arr off len) = FS.cppSubString arr off len

instance NFData Text

instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty = empty
    mconcat = concat

instance IsString Text where
    fromString s = pack s

instance CInput Text where
    type CInputType Text result = CPPString -> result
    withCInput value continuation = continuation (toCppString value)

instance COutput Text where
    type COutputType Text = CPPString
    fromCOutput = fromCppString
