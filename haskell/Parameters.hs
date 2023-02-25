module Parameters where

import Range
import Foreign.Pair       -- for pair_from_c
import Foreign.String     -- for list_to_string
import Effect             -- for Effect

-- An Effect may be a node in a graph??

foreign import bpcall "Modifiables:maybe_modifiable_structure" maybe_modifiable_structure :: a -> a

foreign import bpcall "Modifiables:modifiable" modifiable :: a -> a

foreign import bpcall "Modifiables:register_prior" builtin_register_prior :: Effect -> LogDouble -> RealWorld -> Effect
register_prior event prob = makeIO $ builtin_register_prior event prob

foreign import bpcall "Modifiables:register_likelihood" builtin_register_likelihood :: a -> LogDouble -> RealWorld -> Effect
register_likelihood event prob = makeIO $ builtin_register_likelihood event prob

foreign import bpcall "Modifiables:register_in_edge" builtin_register_in_edge :: a -> Effect -> CPPString -> RealWorld -> Effect
register_in_edge var dist role = makeIO $ builtin_register_in_edge var dist (list_to_string role)

foreign import bpcall "Modifiables:register_out_edge" builtin_register_out_edge :: Effect -> a -> RealWorld -> Effect
register_out_edge dist var = makeIO $ builtin_register_out_edge dist var

-- FIXME: Eliminate the CPPString, and allow each dist to only have one property
foreign import bpcall "Modifiables:register_dist_property" builtin_register_dist_property :: d -> a -> CPPString -> RealWorld -> Effect
register_dist_property dist value property = makeIO $ builtin_register_dist_property dist value (list_to_string property)

foreign import bpcall "Modifiables:register_dist" builtin_register_dist :: CPPString -> Int -> RealWorld -> Effect
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
register_dist_sample  name = makeIO $ builtin_register_dist (list_to_string name) 0
register_dist_observe name = makeIO $ builtin_register_dist (list_to_string name) 1

foreign import bpcall "Modifiables:changeable_apply" ($?) :: (a -> b) -> a -> b

foreign import bpcall "Modifiables:" modifiable_apply :: (a -> b) -> a -> b

io_modifiable action = modifiable_apply unsafePerformIO action

