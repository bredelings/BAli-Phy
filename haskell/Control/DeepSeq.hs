module Control.DeepSeq where

import           Data.Foldable

-- I don't think we can use this for cyclic data structures -- it would just go on forever.
-- I guess for a graph, we could first find all the nodes & edges.

class NFData a where
    -- Reduce to Normal Form
    rnf :: a -> ()
    rnf = rwhnf

infixr 0 `deepseq`

deepseq x y = rnf x `seq` y

force x = x `deepseq` x

-- Reduce to Weak Head Normal Form (WHNF)
rwhnf x = x `seq` ()

instance NFData ()

instance (NFData a, NFData b) => NFData (a,b) where
    rnf (x,y) = rnf x `seq` rnf y

instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
    rnf (x,y,z) = rnf x `seq` rnf y `seq` rnf z

instance (NFData a, NFData b, NFData c, NFData d) => NFData (a,b,c,d) where
    rnf (w,x,y,z) = rnf w `seq` rnf x `seq` rnf y `seq` rnf z 

instance (NFData a, NFData b, NFData c, NFData d, NFData e) => NFData (a,b,c,d,e) where
    rnf (v,w,x,y,z) = rnf v `seq` rnf w `seq` rnf x `seq` rnf y `seq` rnf z 

instance NFData Int

instance NFData Double

instance NFData Char

instance NFData Bool

instance NFData a => NFData (Maybe a) where
    rnf Nothing = ()
    rnf (Just x) = rnf x

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = x `seq` rnf xs


-- Question: in the machine, can we change (x `seq` y `seq` z `seq` ()) to a node that forces x+y+z and evaluates to ()?
-- If y then becomes unreferenced, then this is a win.
-- If we modify each link in the linear chain instead of collapsing the whole chain, then maybe this is a fail?
-- Hmm... we could perform this cleanup if-and-only-if
-- * the reg is an index-var-with-force
-- * the reg is not referenced only by an index-var-with-force
-- * the called reg IS referenced only by an index-var-with-force


instance {-# OVERLAPPABLE #-} (Foldable f, NFData e) => NFData (f e) where
    rnf = foldr seq ()

instance (NFData a,NFData b,NFData c,NFData d,NFData e,NFData f) => NFData (a,b,c,d,e,f) where
    rnf (x1,x2,x3,x4,x5,x6) =
        rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6

instance (NFData a,NFData b,NFData c,NFData d,NFData e,NFData f,NFData g) => NFData (a,b,c,d,e,f,g) where
    rnf (x1,x2,x3,x4,x5,x6,x7) =
        rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7

instance (NFData a,NFData b,NFData c,NFData d,NFData e,NFData f,NFData g,NFData h) => NFData (a,b,c,d,e,f,g,h) where
    rnf (x1,x2,x3,x4,x5,x6,x7,x8) =
        rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8

instance (NFData a,NFData b,NFData c,NFData d,NFData e,NFData f,NFData g,NFData h,NFData i) => NFData
    (a,b,c,d,e,f,g,h,i) where
    rnf (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
        rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9
