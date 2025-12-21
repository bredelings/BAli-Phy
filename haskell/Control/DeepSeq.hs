module Control.DeepSeq where

import           Data.Array
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
