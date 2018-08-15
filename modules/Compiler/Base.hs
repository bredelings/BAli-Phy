{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where
{

builtin error 1 "error" "Prelude";

data IO a = IOAction1 (b->a) a |
            IOAction2 (b->c->a) b c |
            IOAction3 (b->c->d->a) b c d |
            IOAction4 (b->c->d->e->a) b c d e |
            LazyIO a |
            IOReturn a |
            IOAndPass (IO b) (b -> IO a);

infixl 1 >>, >>=;

f >>= g = IOAndPass f g;
f >> g = f >>= (\x -> g);
return f = IOReturn f;
fail e = error e;

infixr 0 $!, `seq`, `join`;
f $! x = x `seq` f x;
builtin seq 2 "seq" "Prelude";
builtin join 2 "join" "Prelude";
}
