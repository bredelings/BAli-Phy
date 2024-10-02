module Data.Text.Display where

import qualified Prelude as P

import Data.Text
import Data.Semigroup

import Foreign.String (show_int, show_integer, show_double)

class Display a where
    display :: a -> Text
    displayList :: [a] -> Text
    -- If we had Builder...
    -- displayList :: [a] -> Builder
    -- displayBuilder :: a -> Builder

    displayList xs = pack "[" <> intercalate (pack ",") (P.map display xs) <> pack "]"

instance Display Text where
    display t = t

instance Display Char where
    display c = pack "'" <> singleton c <> pack "'"
    displayList s = pack "\"" <> pack s <> pack "\""

instance Display Int where
    display i = fromCppString (show_int i)

instance Display Integer where
    display i = fromCppString (show_integer i)

instance Display Double where
    display d = fromCppString (show_double d)

instance Display () where
    display _ = pack "()"

instance (Display a, Display b) => Display (a,b) where
    display (x,y) = pack "(" <> intercalate (pack ",") [display x, display y] <> pack "]"

instance (Display a, Display b, Display c) => Display (a,b,c) where
    display (x,y,z) = pack "(" <> intercalate (pack ",") [display x, display y, display z] <> pack "]"

instance (Display a, Display b, Display c, Display d) => Display (a,b,c,d) where
    display (x,y,z,w) = pack "(" <> intercalate (pack ",") [display x, display y, display z, display w] <> pack "]"

instance Display a => Display [a] where
    display s = displayList s
