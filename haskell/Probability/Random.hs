module Probability.Random (module Probability.Random,
                           module Range,
                           modifiable)
    where

import Range
import Parameters
import MCMC
import Data.JSON as J

-- Define the Distribution type
data Distribution a = Distribution (a->Double) (Double->a) (IO a) Range
densities (Distribution ds _ _ _) = ds
density dist x = balanced_product (densities dist x)
quantile (Distribution _ q _ _) = q
sampler (Distribution _ _ s _) = s
distRange (Distribution _ _ _ r) = r

-- FIXME: We might need GADTS for
--   Independant :: (Random a, Random b) -> Random (a,b)
--   Observe :: b -> (Distribution b) -> Random ()
--   AddMove :: b -> Random ()

-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside Distribution...

data Effects = Effects

-- This implements the Random monad by transforming it into the IO monad.
data Random a = RandomStructure (a->Effects) (a->Effects->a) (Random a)
              | Observe (Distribution b) b
              | AddMove (Int->a)
              | Print b
              | SamplingRate Double (Random a)
              | Lazy (Random a)
              | WithEffect (Random a) (Random a)
              | LiftIO (IO a)

-- I feel sample_with_initial_value actually needs to run the sampler... and make the result come out of that.
-- Maybe this needs to be equivalent to running the regular sample and then setting the value... 
observe = Observe
liftIO = LiftIO
random = Lazy
add_move = AddMove
infixl 2 `with_effect`
with_effect = WithEffect

do_nothing _ = return ()

infix 1 %=%, %=>%
x %=% y      = (x,(Just y,[]))
x %=>% (y,z) = (x,(Just y,z))
infixr 1 %>%
x %>% y      = (x,(Nothing,y))

run_strict (IOAndPass f g) = do
  x <- run_strict f
  run_strict $ g x
run_strict (IOReturn v) = return v
run_strict (LiftIO a) = a
run_strict (Print s) = putStrLn (show s)
run_strict (Lazy r) = run_lazy r


-- The machine should have certain side-effects that can be registered
-- and unregistered, such as registering random_variables, and adding
-- transition kernels.  (LiftIO and Print are here only for debugging
-- purposes.)
--
-- Moving such effects out of the "strict" monad leaves that monad as
-- containing just random sampling and observations.
--
run_effects rate (IOAndPass f g) = let x = run_effects rate f
                                   in x `seq` run_effects rate $ g x
run_effects rate (IOReturn v) = v
-- LiftIO and Print are only here for debugging purposes
-- run_effects alpha rate (LiftIO a) = a
-- run_effects alpha rate (Print s) = putStrLn (show s)
-- FIXME: We don't use the rate here, but we should!
run_effects rate (AddMove m) = register_transition_kernel rate m
run_effects rate (SamplingRate rate2 a) = run_effects (rate*rate2) a

run_lazy (RandomStructure _ _ a) = run_lazy a
run_lazy (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy f
  run_lazy $ g x
run_lazy (IOReturn v) = return v
run_lazy (LiftIO a) = a
run_lazy (Distribution _ _ a _) = unsafeInterleaveIO $ run_lazy a
run_lazy (SamplingRate _ model) = run_lazy model
run_lazy (MFix f) = MFix (run_lazy.f)
run_lazy (WithEffect action _) = run_lazy action

-- Also, shouldn't the modifiable function actually be some kind of monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

run_strict' rate (IOAndPass f g) = do
  x <- run_strict' rate f
  run_strict' rate $ g x
run_strict' rate (IOReturn v) = return v
run_strict' rate (LiftIO a) = a
run_strict' rate (Observe dist datum) = go [register_likelihood term | term <- densities dist datum] `seq` return ()
    where go [] = []
          go (x:xs) = x `seq` go xs
run_strict' rate (Print s) = putStrLn (show s)
run_strict' rate (Lazy r) = run_lazy' rate r

-- 1. Could we somehow implement slice sampling windows for non-contingent variables?

-- NOTE: In order for (run_lazy') to actually be lazy, we need to avoid returning
--       SOMETHING `seq` result.  And this means that we need to frequently
--       intersperse unsafeInterleaveIO to avoid `seq`-ing on previous statements.

modifiable_structure value effect = let x = modifiable value
                                        triggered_x = effect `seq` x
                                    in (x, triggered_x)

run_lazy' rate (LiftIO a) = a
run_lazy' rate (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy' rate f
  run_lazy' rate $ g x
run_lazy' rate (IOReturn v) = return v
run_lazy' rate dist@(Distribution _ _ (RandomStructure effect structure do_sample) range) = do
  -- Note: unsafeInterleaveIO means that we will only execute this line if `value` is accessed.
  value <- unsafeInterleaveIO $ run_lazy do_sample
  let (x,triggered_x) = structure value do_effects
      pdf = density dist x
      rv = random_variable x pdf range rate
      -- Note: performing the rv (i) forces the pdf (a FORCE) and (ii) registers the rv (an EFFECT)
      -- Passing `x` to the effect instead of `rv` maybe should work, but doesn't.
      do_effects = (run_effects rate $ effect rv) `seq` rv
  return triggered_x
run_lazy' rate (Distribution _ _ s _) = run_lazy' rate s
run_lazy' rate (MFix f) = MFix ((run_lazy' rate).f)
run_lazy' rate (SamplingRate rate2 a) = run_lazy' (rate*rate2) a
run_lazy' rate (WithEffect action effect) = unsafeInterleaveIO $ do
  result <- run_lazy' rate action
  let effect_result = run_effects rate $ effect result
  return (effect_result `seq` result)

gen_model_no_alphabet m = run_strict' 1.0 m
add_null_program_result p = do result <- p
                               return (Nothing,result)

add_logger old name (value,[]) False = old
add_logger old name (value,loggers) do_log = (name,(if do_log then Just value else Nothing, loggers)):old

-- Add function to create JSON from logger
log_to_json_one (name,(Nothing,[])) = []
log_to_json_one (name,(Just x,[])) = [(name, to_json x)]
log_to_json_one (name,(Just x,sub_loggers)) = [(name, to_json x), (name++"/", log_to_json sub_loggers)]
log_to_json_one (name,(Nothing,sub_loggers)) = [(name++"/", log_to_json sub_loggers)]

log_to_json loggers = J.Object $ concatMap log_to_json_one loggers

-- Define some helper functions
no_quantile name = error ("Distribution '"++name++"' has no quantile function")
make_densities density x = [density x]

pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balanced_product xs = foldt (*) (doubleToLogDouble 1.0) xs

force_struct x = struct_seq x x

force_list [] = []
force_list (x:xs) = force_struct (x:(force_list xs))

-- maybe I should rename this to (modifiable_list_n n f value) or something.
mapn n f xs = go 0 where
    go i | i==n      = []
         | otherwise = f (xs!!i):go (i+1)
