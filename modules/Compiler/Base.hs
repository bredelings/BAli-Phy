{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where
{

data IO a = IOAction1 (b->a) a |
            IOAction2 (b->c->a) b c |
            IOAction3 (b->c->d->a) b c d |
            IOAction4 (b->c->d->e->a) b c d e |
            LazyIO a |
            IOReturn a |
            IOAndPass (IO b) (b -> IO a);

const x y = x;

infixl 1 >>, >>=;

f >>= g = IOAndPass f g;
f >> g = f >>= (const g);
return f = IOReturn f;
--fail e = error e;
}
