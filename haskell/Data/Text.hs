module Data.Text where

-- Note that this module is really just a hacky implementation of a boxed c++ std::string
-- It should probably be a UTF-8 string, and make Char a UTF-32 value.
-- The number of code points can be larger than the number of graphemes.
-- Does (length) return the number of code points?  Probably.

import Prelude as P
import Data.Char
import Data.Ord

import Foreign.String

data Text = Text CPPString

builtin builtin_pack 1 "pack" "Text"

--

pack = Text . builtin_pack . list_to_vector

unpack (Text s) = unpack_cpp_string s

singleton c = pack [c]

empty = pack []

cons :: Char -> Text -> Text
infixr 5 `cons`
cons c t = pack (c:t') where t' = unpack t

-- snoc :: Text -> Char -> Text 

append :: Text -> Text -> Text
append t1 t2 = pack (unpack t1 ++ unpack t2)

-- uncons :: Text -> Maybe (Char, Text)

-- unsnoc :: Text -> Maybe (Text, Char)

head :: Text -> Char
head t = P.head (unpack t)

last :: Text -> Char
last t = P.last (unpack t)

tail :: Text -> Text
tail t = pack (P.tail $ unpack t)

-- init :: Text -> Text

-- null :: Text -> Bool

-- length :: Text -> Int

-- compareLength :: Text -> Int -> Ordering

-- map :: (Char -> Char) -> Text -> Text

-- intercalate :: Text -> [Text] -> Text

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

-- concat :: [Text] -> Text

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

