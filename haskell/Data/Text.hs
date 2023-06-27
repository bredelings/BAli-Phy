module Data.Text where

-- Note that this module is really just a hacky implementation of a boxed c++ std::string
-- It should probably be a UTF-8 string, and make Char a UTF-32 value.
-- The number of code points can be larger than the number of graphemes.
-- Does (length) return the number of code points?  Probably.

import Prelude as P hiding (null, head, tail, init, length, empty, intercalate, intersperse, concat)
import qualified Prelude as P
import Data.Char
import Data.Ord
import Data.Eq

import Foreign.String

data Text = Text CPPString

foreign import bpcall "Text:pack" builtin_pack :: EVector Char -> CPPString

instance Show Text where
--    show s = show $ unpack s
    show s = "\"" ++ unpack s ++ "\""

pack = Text . builtin_pack . list_to_vector

unpack (Text s) = unpack_cpp_string s

foreign import bpcall "Text:singleton" builtin_singleton :: Char -> CPPString
singleton c = Text $ builtin_singleton c

foreign import bpcall "Text:empty" builtin_empty :: () -> CPPString
empty = Text $ builtin_empty ()

infixr 5 `cons`
foreign import bpcall "Text:cons" builtin_cons :: Char -> CPPString -> CPPString
cons c (Text s) = Text $ builtin_cons c s

foreign import bpcall "Text:snoc" builtin_snoc :: CPPString -> Char -> CPPString
snoc (Text s) c = Text $ builtin_snoc s c

append :: Text -> Text -> Text
foreign import bpcall "Text:append" builtin_append :: CPPString -> CPPString -> CPPString
append (Text s1) (Text s2) = Text $ builtin_append s1 s2

uncons t | null t    = Nothing
         | otherwise = Just (head t, tail t)

-- unsnoc :: Text -> Maybe (Text, Char)

foreign import bpcall "Text:head" builtin_head :: CPPString -> Char
head (Text s) = builtin_head s

foreign import bpcall "Text:last" builtin_last :: CPPString -> Char
last (Text s) = builtin_last s

foreign import bpcall "Text:tail" builtin_tail :: CPPString -> CPPString
tail (Text s) = Text $ builtin_tail s

foreign import bpcall "Text:init" builtin_init :: CPPString -> CPPString
init (Text s) = Text $ builtin_init s

null :: Text -> Bool
null t = length t == 0

foreign import bpcall "Text:length" builtin_length :: CPPString -> Int
length (Text s) = builtin_length s

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

foreign import bpcall "Text:" concatRaw :: EVector CPPString -> CPPString

concat :: [Text] -> Text
concat texts = Text $ concatRaw $ list_to_vector $ map (\(Text s) -> s) texts

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

foreign import bpcall "Text:equals" builtin_equals :: CPPString -> CPPString -> Bool
foreign import bpcall "Text:less_than" builtin_less_than :: CPPString -> CPPString -> Bool

instance Eq Text where
    (Text s1) == (Text s2) = builtin_equals s1 s2

instance Ord Text where
    (Text s1) < (Text s2) = builtin_less_than s1 s2

foreign import bpcall "Prelude:show_double" doubleToTextRaw :: Double -> CPPString
doubleToText d = Text $ doubleToTextRaw d
