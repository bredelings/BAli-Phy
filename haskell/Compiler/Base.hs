{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Foreign.Pair
import Foreign.String

type String = [Char]

builtin_error :: CPPString -> a
foreign import bpcall "Prelude:error" builtin_error 1

error :: [Char] -> a
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
foreign import bpcall "Prelude:seq" seq 2

struct_seq :: a -> b -> b
foreign import bpcall "Prelude:struct_seq" struct_seq 2

