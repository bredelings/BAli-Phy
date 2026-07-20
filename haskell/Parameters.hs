module Parameters where

import Range
import Foreign.String     -- for list_to_string
import Effect             -- for Effect
import Numeric.LogDouble  -- for LogDouble

-- An Effect may be a node in a graph??

foreign import bpcall "Modifiables:" modifiable :: a -> a

foreign import bpcall "Modifiables:" registerPrior :: Effect -> LogDouble -> IO Effect

foreign import bpcall "Modifiables:" registerLikelihood :: a -> LogDouble -> IO Effect

foreign import bpcall "Modifiables:registerInEdge" builtin_registerInEdge :: a -> Effect -> CPPString -> IO Effect
registerInEdge var dist role = builtin_registerInEdge var dist (list_to_string role)

foreign import bpcall "Modifiables:" registerOutEdge :: Effect -> a -> IO Effect

-- FIXME: Eliminate the CPPString, and allow each dist to only have one property
foreign import bpcall "Modifiables:registerDistProperty" builtinRegisterDistProperty :: d -> a -> CPPString -> IO Effect
registerDistProperty dist value property = builtinRegisterDistProperty dist value (list_to_string property)

foreign import bpcall "Modifiables:registerDist" builtinRegisterDist :: CPPString -> Int -> IO Effect
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
registerDistSample  name = builtinRegisterDist (list_to_string name) 0
registerDistObserve name = builtinRegisterDist (list_to_string name) 1

foreign import bpcall "Modifiables:changeable_apply" ($?) :: (a -> b) -> a -> b

-- Make into an IO operation to prevent  floating.
foreign import bpcall "Modifiables:" modifiable_apply :: (a -> b) -> a -> IO b
modifiableIO action = modifiable_apply unsafePerformIO action

