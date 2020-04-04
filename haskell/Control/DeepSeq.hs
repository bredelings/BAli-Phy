module Control.DeepSeq where

import           Foreign.Introspection

infixr 0 `deepseq`

deepseq x y =
    let n = get_n_args x
        go x 0 = get_arg x 0
        go x i = get_arg x i `deepseq` go x (i - 1)
    in  if n > 0
        then go x (n - 1) `seq` y
        else x `seq` y
