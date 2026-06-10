{-# LANGUAGE NoImplicitPrelude #-}
module Text.Read where

import Data.Bool
import Data.Char
import Data.Eq
import Data.Maybe
import Foreign.String
import Foreign.Pair
import Data.List
import Data.Ord
import Compiler.Num
import Compiler.Error

type ReadS a = [Char] -> [(a, [Char])]

class Read a where
    readsPrec :: Int -> ReadS a
    readsPrec _ s = [(read s, [])]
    read :: [Char] -> a
    read s = case [x | (x, rest) <- readsPrec 0 s, all isSpace rest] of
        (x:_) -> x
        [] -> error "Text.Read.read: no parse"

skipSpaces :: [Char] -> [Char]
skipSpaces = dropWhile isSpace

readChar :: Char -> ReadS Bool
readChar c s = case skipSpaces s of
    (x:xs) -> if x == c then [(True, xs)] else []
    [] -> []

readConstructor :: [Char] -> ReadS Bool
readConstructor con s = case stripPrefix con (skipSpaces s) of
    Just rest -> if constructorBoundary rest then [(True, rest)] else []
    Nothing -> []

constructorBoundary :: [Char] -> Bool
constructorBoundary [] = True
constructorBoundary (c:_) = not (isAlphaNum c || c == '_')

readParen :: Bool -> ReadS a -> ReadS a
readParen True p s =
    [(x, s3) | (_, s1) <- readChar '(' s,
               (x, s2) <- p s1,
               (_, s3) <- readChar ')' s2]
readParen False p s = p s ++ readParen True p s

instance Read Int where
    readsPrec _ s =
        let s1 = skipSpaces s
            p = read_int_at (list_to_string s1) 0
            (x, n) = pair_from_c p
        in if n < 0 then [] else [(x, drop n s1)]

instance Read Double where
    readsPrec _ s =
        let s1 = skipSpaces s
            p = read_double_at (list_to_string s1) 0
            (x, n) = pair_from_c p
        in if n < 0 then [] else [(x, drop n s1)]

foreign import ecall "Prelude:" read_int :: CPPString -> Int
foreign import ecall "Prelude:" read_double :: CPPString -> Double
foreign import ecall "Prelude:" read_int_at :: CPPString -> Int -> EPair Int Int
foreign import ecall "Prelude:" read_double_at :: CPPString -> Int -> EPair Double Int
