{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Foreign.Pair
import Foreign.String

type String = [Char]


deep_eval_list [] = []
deep_eval_list (x:xs) = c_pair x (deep_eval_list xs)

builtin_list_to_string :: [Char] -> Int
builtin builtin_list_to_string 1 "list_to_string" "Vector"

list_to_string x = builtin_list_to_string (deep_eval_list x)

builtin_error :: [Char] -> a
builtin builtin_error 1 "error" "Prelude"

error x = builtin_error (list_to_string x)

data IO a = IOAction  (Int->(Int,a)) |
            LazyIO a |
            MFix a |
            IOReturn a |
            IOAndPass (IO Int) (Int -> IO a)

infixl 1 >>, >>=

f >>= g = IOAndPass f g
f >> g = f >>= (\x -> g)
return f = IOReturn f
fail e = error e
mfix f = MFix f

-- join x = x >>= id
join x = do y <- x
            y

infixr 0 $!, `seq`
f $! x = x `seq` f x

seq :: a -> b -> b
builtin seq 2 "seq" "Prelude"

struct_seq :: a -> b -> b
builtin struct_seq 2 "struct_seq" "Prelude"

