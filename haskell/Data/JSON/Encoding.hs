module Data.JSON.Encoding where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Semigroup
import Data.Monoid    

import Data.JSON.Types.Internal
import Data.JSON.Types.ToJSON

import Prelude hiding (empty)
    
class KeyValue kv where
  (.=) :: ToJSON v => Key -> v -> kv
  infixr 8 .=

--instance KeyValue ByteString where
--    k .= v = ??

instance (key ~ Key, value ~ Value) => KeyValue (key, value) where
    k .= v = (k, toJSON v)

-- Apparently tags include Text, Value, and InArray

data InArray -- a tag for Encoding'

data Encoding' tag = Encoding {fromEncoding :: Text}

type Encoding = Encoding' Value

unsafeToEncoding = Encoding

retagEncoding = Encoding . fromEncoding

instance Show (Encoding' a) where
    show (Encoding e) = show e

data Series = Empty | Value (Encoding' Series)

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

comma        = Encoding $ T.singleton ','
colon        = Encoding $ T.singleton  ':'
openBracket  = Encoding $ T.singleton  '['
closeBracket = Encoding $ T.singleton  ']'
openCurly    = Encoding $ T.singleton  '{'
closeCurly   = Encoding $ T.singleton  '}'
