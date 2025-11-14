module Data.JSON.Encoding where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Semigroup
import Data.Monoid    

import Prelude hiding (empty, bool)
import Data.Text.Display (display)

import Data.JSON.Types.Internal (Key(..), Value(..))

-- Apparently tags include Text, Value, and InArray

data InArray -- a tag for Encoding'

data Encoding' tag = Encoding {fromEncoding :: Text}

type Encoding = Encoding' Value

unsafeToEncoding = Encoding

retagEncoding = Encoding . fromEncoding

instance Show (Encoding' a) where
    show (Encoding e) = show e

data Series = Empty | Value (Encoding' Series)

pair :: Key -> Encoding -> Series
pair name val = pair' (key name) val

pairStr :: String -> Encoding -> Series
pairStr name val = pair' (string name) val

pair' :: Encoding' Key -> Encoding -> Series
pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val
                
instance Semigroup Series where
    Empty   <> a = a
    Value a <> b = Value $ a >< case b of
        Empty   -> empty
        Value x -> comma >< x

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding' a -> Encoding' b -> Encoding' InArray
a >*< b = retagEncoding a >< comma >< retagEncoding b

empty = Encoding mempty

econcat :: [Encoding' a] -> Encoding' a
econcat = foldr (><) empty

infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a <> b)

key :: Key -> Encoding' a
key = text . (\(Key t) -> t)

text :: Text -> Encoding' a
text t = Encoding (T.pack ['"'] <> t <> T.pack ['"'])

string :: String -> Encoding' a
string s = text (T.pack s)

emptyArray_ :: Encoding
emptyArray_ = openBracket >< closeBracket               

emptyObject_ :: Encoding
emptyObject_ = openCurly >< closeCurly

wrapArray :: Encoding' a -> Encoding
wrapArray e = retagEncoding $ openBracket >< e >< closeBracket

wrapObject :: Encoding' a -> Encoding
wrapObject e = retagEncoding $ openCurly >< e >< closeCurly

bool True = Encoding (T.pack "true")
bool False = Encoding (T.pack "false")

pairs :: Series -> Encoding
pairs (Value v) = openCurly >< retagEncoding v >< closeCurly
pairs Empty     = emptyObject_

list :: (a -> Encoding) -> [a] -> Encoding
list _ [] = emptyArray_
list to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
    where
      commas = foldr (\v vs -> comma >< to' v >< vs) empty

dict
    :: (k -> Encoding' Key)                           -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = pairs . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = retagEncoding (encodeKey k) >< colon >< retagEncoding (encodeVal v)

null_ = Encoding (T.pack "null")

eobject [] = emptyObject_
eobject (x:xs) = openCurly >< one x >< rest xs 
    where
      rest (y:ys) = comma >< one y >< rest ys
      rest []     = closeCurly
      one (k,v) = key k >< colon >< value v

array [] = emptyArray_
array (x:xs) = openBracket >< value x >< rest xs
    where
      rest (y:ys) = comma >< value y >< rest ys
      rest [] = closeBracket

-- chars

comma        = Encoding $ T.singleton ','
colon        = Encoding $ T.singleton ':'
openBracket  = Encoding $ T.singleton '['
closeBracket = Encoding $ T.singleton ']'
openCurly    = Encoding $ T.singleton '{'
closeCurly   = Encoding $ T.singleton '}'

-- numbers

int :: Int -> Encoding
int = Encoding . display
-- integer :: Integer -> Encoding
-- float :: Float -> Encoding

double :: Double -> Encoding
double = Encoding . display

value :: Value -> Encoding
value (Array xs) = array xs
value (Object kvs) = eobject kvs
value (INumber i) = int i
value (FNumber d) = double d
value (Bool b) = bool b
value (String t) = text t
value Null = null_
