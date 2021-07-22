module Probability.Random (module Probability.Random,
                           module Range,
                           modifiable)
    where

import Range
import Parameters
import MCMC
import Data.JSON as J

data SamplingEvent
data ProbEventMonad = InEdge String a | PropertyEdge String a | ProbFactor Double
in_edge node name = InEdge node name
property node name = PropertyEdge node name

-- Just get the densities out
-- No ProbFactor events yet.
get_densities (IOReturn x) = x
get_densities (IOAndPass f g) = let x = get_densities f in get_densities (g x)
get_densities (InEdge _ x) = x
get_densities (PropertyEdge _ _) = ()

make_edges event (IOReturn x) = return x
make_edges event (IOAndPass f g) = do x <- make_edges event f
                                      make_edges event (g x)
make_edges event (InEdge name node) = register_in_edge node event name
make_edges event (PropertyEdge name node) = register_dist_property event node name

-- Define the Distribution type
data Distribution a = Distribution String (a->Double) (Double->a) (IO a) Range
dist_name (Distribution n _ _ _ _) = n
annotated_densities (Distribution _ ds _ _ _) = ds
densities dist x = get_densities $ annotated_densities dist x
density dist x = balanced_product (densities dist x)
quantile (Distribution _ _ q _ _) = q
sampler (Distribution _ _ _ s _) = s
distRange (Distribution _ _ _ _ r) = r

-- FIXME: We might need GADTS for
--   Independant :: (Random a, Random b) -> Random (a,b)
--   Observe :: b -> (Distribution b) -> Random ()
--   AddMove :: b -> Random ()

-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside Distribution...

data TKEffects a = SamplingRate Double (Random a) | AddMove (Int->a)

-- This implements the Random monad by transforming it into the IO monad.
data Random a = RandomStructure (a->TKEffects) (a->TKEffects->a) (Random a)
              | Observe (Distribution b) b
              | Print b
              | Lazy (Random a)
              | WithTKEffect (Random a) (TKEffects b)
              | PerformTKEffect (TKEffects a)
              | LiftIO (IO a)

-- I feel sample_with_initial_value actually needs to run the sampler... and make the result come out of that.
-- Maybe this needs to be equivalent to running the regular sample and then setting the value... 
observe = Observe
x ~> dist = observe dist x
infix 0 ~>

liftIO = LiftIO
lazy = Lazy
sample = Lazy
add_move = AddMove
perform_tk_effect = PerformTKEffect
infixl 2 `with_tk_effect`
with_tk_effect = WithTKEffect

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


run_tk_effects rate (IOAndPass f g) = (run_tk_effects rate f) >>= (\x -> run_tk_effects rate $ g x)
run_tk_effects rate (IOReturn v) = return v
run_tk_effects rate (AddMove m) = register_transition_kernel rate m
run_tk_effects rate (SamplingRate rate2 a) = run_tk_effects (rate*rate2) a
-- LiftIO and Print are only here for debugging purposes:
--  run_tk_effects alpha rate (LiftIO a) = a
--  run_tk_effects alpha rate (Print s) = putStrLn (show s)


run_lazy (RandomStructure _ _ a) = run_lazy a
run_lazy (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy f
  run_lazy $ g x
run_lazy (IOReturn v) = return v
run_lazy (LiftIO a) = a
run_lazy (Distribution _ _ _ a _) = unsafeInterleaveIO $ run_lazy a
run_lazy (SamplingRate _ model) = run_lazy model
run_lazy (MFix f) = MFix (run_lazy.f)
run_lazy (WithTKEffect action _) = run_lazy action

-- Also, shouldn't the modifiable function actually be some kind of monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

run_strict' rate (IOAndPass f g) = do
  x <- run_strict' rate f
  run_strict' rate $ g x
run_strict' rate (IOReturn v) = return v
run_strict' rate (LiftIO a) = a
run_strict' rate (Observe dist datum) = do
  s <- register_dist_observe (dist_name dist)
  register_out_edge s datum
  density_terms <- make_edges s $ annotated_densities dist datum
  sequence_ [register_likelihood s term | term <- density_terms]
run_strict' rate (Print s) = putStrLn (show s)
run_strict' rate (PerformTKEffect e) = run_tk_effects rate e
run_strict' rate (AddMove m) = register_transition_kernel rate m
run_strict' rate (SamplingRate rate2 a) = run_strict' (rate*rate2) a
-- These are the lazily executed parts of the strict monad.
run_strict' rate dist@(Distribution _ _ _ _ _) = unsafeInterleaveIO $ run_lazy' rate dist
run_strict' rate (MFix f) = unsafeInterleaveIO $ MFix ((run_lazy' rate).f)
run_strict' rate (Lazy r) = unsafeInterleaveIO $ run_lazy' rate r

-- 1. Could we somehow implement slice sampling windows for non-contingent variables?

-- NOTE: In order for (run_lazy') to actually be lazy, we need to avoid returning
--       SOMETHING `seq` result.  And this means that we need to frequently
--       intersperse unsafeInterleaveIO to avoid `seq`-ing on previous statements.

triggered_modifiable_structure mod_structure force_structure value effect = (raw_x, triggered_x)
    where raw_x       = mod_structure modifiable value
          effect'     = force_structure raw_x `seq` effect
          triggered_x = mod_structure (effect' `seq`) raw_x

modifiable_structure = triggered_modifiable_structure ($) id

-- It seems like we could return raw_x in most cases, except the case of a tree.
-- But in the tree case, we could return triggered_x.

-- Note on unsafeInterleaveIO:
--       Simply using run_lazy does not guarantee that the result of run_lazy
--       will not be demanded.  (It guarantees that f >>= g that are INSIDE run_lazy
--       won't demand the result of the f).
--       We need to guard any IO operations with unsafeInterleaveIO if we
--       want to prevent their results from being demanded.
--
-- QUESTION: So, do we need to guard the execution of Distributions with unsafeInterleaveIO?
-- ANSWER: No.  If its not the last entry in a sequence, it will get unsafeInterleaveIO from
--         run_lazy' _ (IOAndPass _ _).
--         If it is run from run_strict' directly, then it is run with
--         unsafeInterleaveIO $ run_lazy', so we get an unsafeInterleaveIO from there.
--
run_lazy' rate (LiftIO a) = a
run_lazy' rate (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy' rate f
  run_lazy' rate $ g x
run_lazy' rate (IOReturn v) = return v
run_lazy' rate dist@(Distribution _ _ _ (RandomStructure tk_effect structure do_sample) range) = do 
 -- Note: unsafeInterleaveIO means that we will only execute this line if `value` is accessed.
  value <- unsafeInterleaveIO $ run_lazy do_sample
  let (raw_x,triggered_x) = structure value do_effects
      effect = do
        run_tk_effects rate $ tk_effect raw_x
        s <- register_dist_sample (dist_name dist)
        density_terms <- make_edges s $ annotated_densities dist raw_x
        sequence_ [register_prior s term | term <- density_terms]
        register_out_edge s raw_x
      do_effects = unsafePerformIO effect
  return triggered_x
run_lazy' rate (Distribution _ _ _ s _) = run_lazy' rate s
run_lazy' rate (MFix f) = MFix ((run_lazy' rate).f)
run_lazy' rate (SamplingRate rate2 a) = run_lazy' (rate*rate2) a
run_lazy' rate (Lazy r) = run_lazy' rate r
run_lazy' rate (WithTKEffect action tk_effect) = unsafeInterleaveIO $ do
  result <- unsafeInterleaveIO $ run_lazy' rate action
  run_tk_effects rate $ tk_effect result
  return result

gen_model_no_alphabet m = run_strict' 1.0 m
mcmc = gen_model_no_alphabet
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
make_densities density x = return [density x]
make_densities' densities x = return $ densities x
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
