{-# LANGUAGE NoImplicitPrelude #-}
module Text.Show
    (
    Show(..), ShowS,
    shows, showChar, showString, showParen, showListWith
    )
where

import Compiler.Base
import Foreign.String
import Data.List
import Data.Function
import Compiler.Num
import Data.Ord
import Data.Char (ord)    

type ShowS = String -> String


class Show a where
    showsPrec :: Int -> a -> ShowS
    show :: a -> String
    showList :: [a] -> ShowS

    showsPrec _ x s = show x ++ s
    show x = shows x ""
    showList = showListWith shows


showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith showsx [] = showString "[]"
showListWith showsx (x:y) = showChar '[' . showsx x . shows' y
        where shows' [] = showChar ']'
              shows' (x:y) = showChar ',' . showsx x . shows' y

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p



showLitChar :: Char -> ShowS
-- | c > '\DEL'
-- '\DEL'
showLitChar '\\' s             = showString "\\\\" s
showLitChar c    s  | c >= ' ' = showChar c s
showLitChar '\a' s             = showString "\\a" s
showLitChar '\b' s             = showString "\\b" s
showLitChar '\f' s             = showString "\\f" s
showLitChar '\n' s             = showString "\\n" s
showLitChar '\r' s             = showString "\\r" s
showLitChar '\t' s             = showString "\\t" s
showLitChar '\v' s             = showString "\\v" s
-- | '\SO'
showLitChar c     s            = showString ('\\' : asciiTab!!ord c) s


asciiTab :: [String]
asciiTab = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]


                                 
showLitString :: String -> ShowS
showLitString []         s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c   : cs) s = showLitChar c (showLitString cs s)

-- Show chars and strings, with quotes and escaped characters.
instance Show Char where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showLitString cs . showChar '"'

instance Show Int where
    -- Modify this to put parenthesis around negative integers if p > 6
    show i = unpack_cpp_string $ show_int i

instance Show Integer where
    show i = unpack_cpp_string $ show_integer i

instance Show Double where
    show  d = unpack_cpp_string $ show_double d

instance Show () where
    show _ = "()"

instance (Show a, Show b) => Show (a,b) where
    show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance (Show a, Show b, Show c) => Show (a,b,c) where
    show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where
    show (x,y,z,w) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w ++ ")"

instance Show a => Show [a] where
    showsPrec _ = showList

instance (Show a,Show b,Show c,Show d,Show e) => Show (a,b,c,d,e) where
    show (x1,x2,x3,x4,x5) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f) => Show (a,b,c,d,e,f) where
    show (x1,x2,x3,x4,x5,x6) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g) => Show (a,b,c,d,e,f,g) where
    show (x1,x2,x3,x4,x5,x6,x7) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h) => Show (a,b,c,d,e,f,g,h) where
    show (x1,x2,x3,x4,x5,x6,x7,x8) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i) => Show (a,b,c,d,e,f,g,h,i) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j) => Show (a,b,c,d,e,f,g,h,i,j) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k) => Show
    (a,b,c,d,e,f,g,h,i,j,k) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ "," ++ show x11 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l) => Show
    (a,b,c,d,e,f,g,h,i,j,k,l) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ "," ++ show x11 ++ "," ++ show x12
        ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m) => Show
    (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ "," ++ show x11 ++ "," ++ show x12
        ++ "," ++ show x13 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m,Show n) =>
    Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ "," ++ show x11 ++ "," ++ show x12
        ++ "," ++ show x13 ++ "," ++ show x14 ++ ")"

instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j,Show k,Show l,Show m,Show n,Show o)
    => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    show (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) =
        "(" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "," ++ show x6 ++
        "," ++ show x7 ++ "," ++ show x8 ++ "," ++ show x9 ++ "," ++ show x10 ++ "," ++ show x11 ++ "," ++ show x12
        ++ "," ++ show x13 ++ "," ++ show x14 ++ "," ++ show x15 ++ ")"

