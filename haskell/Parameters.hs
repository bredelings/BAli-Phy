module Parameters where

import Range
import Foreign.String     -- for list_to_string
import Effect             -- for Effect
import Numeric.LogDouble  -- for LogDouble

-- An Effect may be a node in a graph??

foreign import bpcall "Modifiables:maybe_modifiable_structure" maybe_modifiable_structure :: a -> a

foreign import bpcall "Modifiables:" modifiable :: a -> a

foreign import bpcall "Modifiables:" register_prior :: Effect -> LogDouble -> IO Effect

foreign import bpcall "Modifiables:" register_likelihood :: a -> LogDouble -> IO Effect

foreign import bpcall "Modifiables:register_in_edge" builtin_register_in_edge :: a -> Effect -> CPPString -> IO Effect
register_in_edge var dist role = builtin_register_in_edge var dist (list_to_string role)

foreign import bpcall "Modifiables:" register_out_edge :: Effect -> a -> IO Effect

-- FIXME: Eliminate the CPPString, and allow each dist to only have one property
foreign import bpcall "Modifiables:register_dist_property" builtin_register_dist_property :: d -> a -> CPPString -> IO Effect
register_dist_property dist value property = builtin_register_dist_property dist value (list_to_string property)

foreign import bpcall "Modifiables:register_dist" builtin_register_dist :: CPPString -> Int -> IO Effect
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
register_dist_sample  name = builtin_register_dist (list_to_string name) 0
register_dist_observe name = builtin_register_dist (list_to_string name) 1

foreign import bpcall "Modifiables:changeable_apply" ($?) :: (a -> b) -> a -> b

-- Make into an IO operation to prevent  floating.
foreign import bpcall "Modifiables:" modifiable_apply :: (a -> b) -> a -> IO b
modifiableIO action = modifiable_apply unsafePerformIO action

