{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Foreign.Pair

deep_eval_list [] = []
deep_eval_list (x:xs) = c_pair x (deep_eval_list xs)

builtin builtin_list_to_string 1 "list_to_string" "Vector"
list_to_string x = builtin_list_to_string (deep_eval_list x)

builtin builtin_error 1 "error" "Prelude"
error x = builtin_error (list_to_string x)

data IO a = IOAction  (s->(s,a)) |
            LazyIO a |
            MFix a |
            IOReturn a |
            IOAndPass (IO b) (b -> IO a)

infixl 1 >>, >>=

f >>= g = IOAndPass f g
f >> g = f >>= (\x -> g)
return f = IOReturn f
fail e = error e
mfix f = MFix f

infixr 0 $!, `seq`, `join`
f $! x = x `seq` f x
builtin seq 2 "seq" "Prelude"
builtin join 2 "join" "Prelude"
builtin struct_seq 2 "struct_seq" "Prelude"

