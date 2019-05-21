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
--   GetAlphabet :: Random b ?
--   SetAlphabet :: b -> (Random a) -> Random a
--   AddMove :: b -> Random ()

-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside Distribution...

-- This implements the Random monad by transforming it into the IO monad.
data Random a = RandomStructure b a (Random a)
              | RandomStructureAndPDF b a (Random a)
              | Sample (Distribution a)
              | SampleWithInitialValue (Distribution a) a
              | Observe (Distribution b) b
              | AddMove (Int->a)
              | Print b
              | SamplingRate Double (Random a)
              | GetAlphabet
              | SetAlphabet b (Random a)
              | Lazy (Random a)
              | Strict (Random a)
              | LiftIO (IO a)


sample dist = Sample dist
sample_with_initial_value dist value = SampleWithInitialValue dist value
observe = Observe
liftIO = LiftIO
random = Lazy
block  = Strict
add_move = AddMove

log_all loggers = (Nothing,loggers)

infix 1 %%
x %% y = (y,(Just x,[]))

run_strict alpha (IOAndPass f g) = do
  x <- run_strict alpha f
  run_strict alpha $ g x
run_strict alpha (IOReturn v) = return v
run_strict alpha (LiftIO a) = a
run_strict alpha GetAlphabet = return alpha
run_strict alpha (SetAlphabet a2 x) = run_strict a2 x
run_strict alpha (Print s) = putStrLn (show s)
run_strict alpha (Lazy r) = run_lazy alpha r


-- The machine should have certain side-effects that can be registered
-- and unregistered, such as registering random_variables, and adding
-- transition kernels.  (LiftIO and Print are here only for debugging
-- purposes.)
--
-- Moving such effects out of the "strict" monad leaves that monad as
-- containing just random sampling and observations.
--
run_effects alpha rate (IOAndPass f g) = do
  x <- run_effects alpha rate f
  run_effects alpha rate $ g x
run_effects alpha rate (IOReturn v) = return v
-- LiftIO and Print are only here for debugging purposes
run_effects alpha rate (LiftIO a) = a
run_effects alpha rate (Print s) = putStrLn (show s)
-- FIXME: We don't use the rate here, but we should!
run_effects alpha rate (AddMove m) = register_transition_kernel m
run_effects alpha rate (SamplingRate rate2 a) = run_effects alpha (rate*rate2) a

run_lazy alpha (RandomStructure _ _ a) = run_lazy alpha a
run_lazy alpha (RandomStructureAndPDF _ _ a) = run_lazy alpha a
run_lazy alpha (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy alpha f
  run_lazy alpha $ g x
run_lazy alpha (IOReturn v) = return v
run_lazy alpha (LiftIO a) = a
run_lazy alpha (Sample (Distribution _ _ a _)) = unsafeInterleaveIO $ run_lazy alpha a
run_lazy alpha (SampleWithInitialValue (Distribution _ _ a _) _) = unsafeInterleaveIO $ run_lazy alpha a
run_lazy alpha GetAlphabet = return alpha
run_lazy alpha (SetAlphabet a2 x) = run_lazy a2 x
run_lazy alpha (SamplingRate _ model) = run_lazy alpha model
run_lazy alpha (MFix f) = MFix ((run_lazy alpha).f)
run_lazy alpha (Strict r) = run_strict alpha r


-- Q: Isn't Sample a special case of SampleWithInitialValue?
-- A: No... we need to run the do_sample routine in the plain interpreter.

-- Isn't Random (almost) a special case of RandomStructure, with structure = modifiable?
--
-- Maybe we need an RandomIO action to just perform IO actions inside the random monad?  What is normal Haskell terminology for this?
--
-- Also, shouldn't the modifiable function actually be in the IO monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

--
-- Plan: We can implement lazy interpretation by add a (Strict action) constructor to Random, and modifying (IOAndPass f g) to
--       do unsafeInterleaveIO if f does not match (Strict f')

run_strict' alpha rate (IOAndPass f g) = do
  x <- run_strict' alpha rate f
  run_strict' alpha rate $ g x
run_strict' alpha rate (IOReturn v) = return v
run_strict' alpha rate (LiftIO a) = a
run_strict' alpha _    GetAlphabet = return alpha
run_strict' alpha rate (SetAlphabet a2 x) = run_strict' a2 rate x
run_strict' alpha rate (Observe dist datum) = sequence_ [register_likelihood term | term <- densities dist datum]
run_strict' alpha rate (Print s) = putStrLn (show s)
run_strict' alpha rate (Lazy r) = run_lazy' alpha rate r

-- 1. If we add transition kernel INSIDE the SampleWithInitialValue move, then maybe
--    we could STOP annotation each random variable with `range` and `rate`.

-- 2. Could we somehow implement slice sampling windows for non-contingent variables?

do_nothing x pdf rate = return ()

infixl 2 `with_effect`

with_effect (Distribution pdf quantile (RandomStructureAndPDF effect1 structure_and_pdf sample_action) range) effect2 =
    let effect1_and_2 x pdf rate = effect1 x pdf rate >> effect2 x pdf rate
    in Distribution pdf quantile (RandomStructureAndPDF effect1_and_2 structure_and_pdf sample_action) range

with_effect (Distribution pdf quantile (RandomStructure effect1 structure sample_action) range) effect2 =
    let effect1_and_2 x pdf rate = effect1 x pdf rate >> effect2 x pdf rate
    in Distribution pdf quantile (RandomStructure effect1_and_2 structure sample_action) range

run_lazy' alpha rate (LiftIO a) = a
run_lazy' alpha rate (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy' alpha rate f
  run_lazy' alpha rate $ g x
run_lazy' alpha rate (IOReturn v) = return v
run_lazy' alpha rate (Sample dist@(Distribution _ _ (RandomStructure _ _ do_sample) range)) = do
  value <- run_lazy alpha do_sample
  run_lazy' alpha rate $ SampleWithInitialValue dist value
run_lazy' alpha rate (Sample dist@(Distribution _ _ (RandomStructureAndPDF _ _ do_sample) range)) = do
  value <- run_lazy alpha do_sample
  run_lazy' alpha rate $ SampleWithInitialValue dist value
run_lazy' alpha rate (SampleWithInitialValue dist@(Distribution _ _ (RandomStructure effect structure do_sample) range) initial_value) = unsafeInterleaveIO $ do
  -- maybe we need to perform the sample and not use the result, in order to force the parameters of the distribution? 
  -- we need some mcmc moves here, for crp and for trees
  let x = structure initial_value
      pdf = density dist x
  run_effects alpha rate $ effect x pdf rate
  return $ random_variable x pdf range rate
run_lazy' alpha rate (SampleWithInitialValue dist@(Distribution _ _ (RandomStructureAndPDF effect structure_and_pdf do_sample) range) initial_value) = unsafeInterleaveIO $ do
  -- maybe we need to perform the sample and not use the result, in order to force the parameters of the distribution? 
  -- we need some mcmc moves here, for crp and for trees
  let (x,pdf) = structure_and_pdf initial_value rv
      rv = random_variable x pdf range rate
  run_effects alpha rate $ effect x pdf rate
  return x
run_lazy' alpha rate (Sample (Distribution _ _ s _)) = run_lazy' alpha rate s
run_lazy' alpha rate (MFix f) = MFix ((run_lazy' alpha rate).f)
run_lazy' alpha rate (SamplingRate rate2 a) = run_lazy' alpha (rate*rate2) a
run_lazy' alpha _    GetAlphabet = return alpha
run_lazy' alpha rate (SetAlphabet a2 x) = run_lazy' a2 rate x
run_lazy' alpha rate (Strict r) = run_strict' alpha rate r

set_alphabet a x = do (a',_) <- a
                      SetAlphabet a' x

set_alphabet' = SetAlphabet

gen_model_no_alphabet m = run_lazy' (error "No default alphabet!") 1.0 m

add_logger old name (value,[]) False = old
add_logger old name (value,loggers) do_log = (name,(if do_log then Just value else Nothing, loggers)):old

-- Add function to create JSON from logger
log_to_json_one (name,(Nothing,[])) = []
log_to_json_one (name,(Just x,[])) = [(name, J.Object[("value", to_json x)])]
log_to_json_one (name,(Just x,sub_loggers)) = [(name, J.Object[("value", to_json x),("children", log_to_json sub_loggers)])]
log_to_json_one (name,(Nothing,sub_loggers)) = [(name, J.Object[("children", log_to_json sub_loggers)])]

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

