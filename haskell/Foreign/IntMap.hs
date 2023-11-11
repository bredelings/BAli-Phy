module Foreign.IntMap where

import Prelude hiding ((!))
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)

data EIntMap a

foreign import bpcall "IntMap:ekeysSet" keysSet :: EIntMap a -> IntSet

foreign import bpcall "IntMap:esubscript" (!) :: EIntMap a -> Int -> a

importIntMap m = keysSet m & IM.fromSet (m !)
