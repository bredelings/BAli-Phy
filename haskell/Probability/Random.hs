module Probability.Random (module Probability.Random,
                           module Range,
                           module Numeric.LogDouble,
                           module Numeric.Prob,
                           module Probability.Dist,
                           modifiable,
                           liftIO,
                           withEffect)
    where

import Range
import Parameters
import MCMC
import Data.JSON as J
import Effect
import Control.Monad.IO.Class -- for liftIO
import Data.IntMap (IntMap)
import Data.Array (Array)
import Numeric.LogDouble
import Numeric.Prob
import Control.Monad.Fix

import           System.IO
import qualified Data.Text.IO as TIO

import Probability.Dist

data AnnotatedDensity a where
    InEdge :: String -> b -> AnnotatedDensity ()
    PropertyEdge :: String -> b -> AnnotatedDensity ()
    ADBind :: AnnotatedDensity b -> (b -> AnnotatedDensity a) -> AnnotatedDensity a
    ADReturn :: a -> AnnotatedDensity a
    ProbFactor :: Double -> AnnotatedDensity ()

in_edge name node = InEdge name node
property name node = PropertyEdge name node


instance Functor AnnotatedDensity where
    fmap f x = ADBind x (\result -> ADReturn (f result))

instance Applicative AnnotatedDensity where
    pure  x = ADReturn x
    f <*> x = ADBind x (\x' -> ADBind f (\f' -> pure (f' x')))

instance Monad AnnotatedDensity where
    f >>= g = ADBind f g


-- Just get the densities out
-- No ProbFactor events yet.
get_densities :: AnnotatedDensity a -> a
get_densities (ADReturn x) = x
get_densities (ADBind f g) = let x = get_densities f in get_densities (g x)
get_densities (InEdge _ x) = ()
get_densities (PropertyEdge _ x) = ()


make_edges :: Effect -> AnnotatedDensity a -> IO a
make_edges event (ADReturn x) = return x
make_edges event (ADBind f g) = do x <- make_edges event f
                                   make_edges event (g x)
make_edges event (InEdge name node) = do register_in_edge node event name
                                         return ()
make_edges event (PropertyEdge name node) = do register_dist_property event node name
                                               return ()

-- Define the Distribution type
densities dist x = get_densities $ annotated_densities dist x
density dist x = balanced_product (densities dist x)

-- can we change all of these ^^ functions into member functions?

-- We can observe from these.
class Dist d => HasAnnotatedPdf d where
    annotated_densities :: d -> Result d -> AnnotatedDensity [LogDouble]

-- We know how to sample from these -- theres a default effect?
class Dist d => Sampleable d where
    sample :: d -> Random (Result d)

instance Dist (Random a) where
    type Result (Random a) = a

instance IOSampleable (Random a) where
    sampleIO dist = run_lazy dist

instance Sampleable (Random a) where
    sample r = r

prior = sample

-- Maybe we would just define `normal mu sigma = sample $ Normal mu sigma`?
-- How about the effect?



-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside Distribution...

data TKEffects a = SamplingRate Double (TKEffects a)
                 | TKLiftIO (Double -> IO a)
                 | TKReturn a
                 | forall b . TKBind (TKEffects b) (b -> TKEffects a)

instance Functor TKEffects where
    fmap f x = TKBind x (\result -> TKReturn (f result))

instance Applicative TKEffects where
    pure  x = TKReturn x
    f <*> x = TKBind x (\x' -> TKBind f (\f' -> pure (f' x')))

instance Monad TKEffects where
    return x = TKReturn x
    f >>= g  = TKBind f g

data Random a where
    RanOp :: ((forall b.Random b -> IO b) -> IO a) -> Random a
    RanBind :: Random b -> (b -> Random a) -> Random a
    Lazy :: Random a -> Random a
    WithTKEffect :: Random a -> (a -> TKEffects b) -> Random a
    PerformTKEffect :: TKEffects a -> Random a
    RanDistribution2 :: (IOSampleable d, HasAnnotatedPdf d) => d -> (Result d -> TKEffects b) -> Random (Result d)
    RanDistribution3 :: HasAnnotatedPdf d => d -> ((Result d)->TKEffects b) -> ((Result d) -> ((Result d) -> IO ()) -> (Result d)) -> Random (Result d) -> Random (Result d)
    RanSamplingRate :: Double -> Random a -> Random a
    RanInterchangeable :: Random b -> Random (Random b)
{-
We need the non-RanOp constructors because they behave differently in the different interpreters.
- RanOp: does the same thing in all interpreters.
- RanBind: adds unsafeInterleaveIO in the lazy interpreters, but not the strict ones.
- Lazy: doesn't just add unsafeInterleaveIO --> also forwards to a different interpreter.
- WithTKEffect: ignores the effect in the simple interpreters, runs the effect in the MCMC interpreters.
- PerformTKEffect: runs the effect at rate 1.0 in the simple interpreters, but at rate `rate` in the MCMC interpreters.
- RanDistribution2 just runs sampleIO in the simple interpreters, but creates a modifiable that runs sampleIO in the MCMC interpreters.
- RanDistribution3 just runs the sampling action in the simple interpreters, but creates a random structure and
   runs the associate tk_effects in the mcmc interpreter.
- RanSamplingRate: does nothing in the simple interpreters, but affects the rate of TKs in the MCMC interpreters
- RanInterchangeable: errors out in the strict interpeters, does nothing in the lazy simple interpreter, and
  creates an interchangable random dist in the lazy MCMC interpreter.
-}


instance Functor Random where
    fmap f r = RanBind r (return . f)

instance Applicative Random where
    pure  x = RanOp (\interp -> return x)
    f <*> x = RanBind x (\x' -> RanBind f (\f' -> pure (f' x')))

instance Monad Random where
    f >>= g  = RanBind f g
--    f >>= g  = RanOp (\i -> (i f) >>= (i . g))
--    ^^ This doesn't work for the lazy interpreters because its strict.

instance MonadFix Random where
    mfix f   = RanOp (\interp -> mfix $ interp . f)

observe datum dist = liftIO $ do
                       s <- register_dist_observe (dist_name dist)
                       register_out_edge s datum
                       density_terms <- make_edges s $ annotated_densities dist datum
                       sequence_ [register_likelihood s term | term <- density_terms]

infix 0 `observe`

instance MonadIO Random where
    liftIO io = RanOp (\interp -> io)

lazy = Lazy
interchangeable = RanInterchangeable
infixl 2 `with_tk_effect`
with_tk_effect = WithTKEffect

addLogger logger = liftIO $ register_logger logger

do_nothing _ = return ()

run_strict :: Random a -> IO a
run_strict (RanBind f g) = do
  x <- run_strict f
  run_strict $ g x
run_strict (RanSamplingRate _ a) = run_strict a
run_strict (RanInterchangeable r) = return r
-- These are the lazily executed parts of the strict monad.
run_strict dist@(RanDistribution2 _ _) = run_lazy dist
run_strict dist@(RanDistribution3 _ _ _ _) = run_lazy dist
run_strict e@(WithTKEffect _ _) = run_lazy e
run_strict (Lazy r) = unsafeInterleaveIO $ run_lazy r
run_strict (RanInterchangeable _) = error "run_strict: RanInterchangeable"
run_strict (PerformTKEffect _) = error "run_strict: PerformTKEffect"
run_strict (RanOp op) = op run_strict

add_move m = TKLiftIO $ (\rate -> register_transition_kernel rate m)

run_tk_effects :: Double -> TKEffects a -> IO a
run_tk_effects rate (TKBind f g) = do x <- run_tk_effects rate f
                                      run_tk_effects rate $ g x
run_tk_effects rate (TKReturn v) = return v
run_tk_effects rate (TKLiftIO action) = action rate
run_tk_effects rate (SamplingRate rate2 a) = run_tk_effects (rate*rate2) a
-- LiftIO and Print are only here for debugging purposes:
--  run_tk_effects alpha rate (LiftIO a) = a
--  run_tk_effects alpha rate (Print s) = putStrLn (show s)

run_lazy :: Random a -> IO a
run_lazy (RanBind f g) = do
  x <- unsafeInterleaveIO $ run_lazy f
  run_lazy $ g x
run_lazy (RanSamplingRate _ a) = run_lazy a
-- Problem: distributions aren't part of the Random monad!
run_lazy (RanDistribution2 dist _) = unsafeInterleaveIO $ sampleIO dist
run_lazy (RanDistribution3 _ _ _ do_sample) = unsafeInterleaveIO $ run_lazy do_sample
run_lazy (PerformTKEffect e) = run_tk_effects 1.0 e
run_lazy (WithTKEffect action _) = run_lazy action
run_lazy (Lazy a) = run_lazy a
run_lazy (RanInterchangeable a) = return a
run_lazy (RanOp op) = op run_lazy

-- Also, shouldn't the modifiable function actually be some kind of monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

run_strict' :: Double -> Random a -> IO a
run_strict' rate (RanBind f g) = do
  x <- run_strict' rate f
  run_strict' rate $ g x
run_strict' rate (PerformTKEffect e) = run_tk_effects rate e
run_strict' rate (RanSamplingRate rate2 a) = run_strict' (rate*rate2) a
-- These are the lazily executed parts of the strict monad.
run_strict' rate ix@(RanInterchangeable r) = run_lazy' rate ix
run_strict' rate dist@(RanDistribution2 _ _) = run_lazy' rate dist
run_strict' rate dist@(RanDistribution3 _ _ _ _) = run_lazy' rate dist
run_strict' rate e@(WithTKEffect _ _) = run_lazy' rate e
run_strict' rate (Lazy r) = unsafeInterleaveIO $ run_lazy' rate r
run_strict' rate (RanOp op) = op (run_strict' rate)

-- NOTE: In order for (run_lazy') to actually be lazy, we need to avoid returning
--       SOMETHING `seq` result.  And this means that we need to frequently
--       intersperse unsafeInterleaveIO to avoid `seq`-ing on previous statements.

triggered_modifiable_structure :: ((forall a.a -> a) -> b -> b) -> b -> (b -> IO ()) -> b
triggered_modifiable_structure mod_structure value effect = triggered_x
    where raw_x       = mod_structure modifiable value
          effect'     = unsafePerformIO $ effect raw_x
          triggered_x = mod_structure (withEffect effect') raw_x

apply_modifier :: (forall a.a -> a) -> b -> b
apply_modifier x y = x y

modifiable_structure :: b -> (b -> IO ()) -> b
modifiable_structure = triggered_modifiable_structure apply_modifier

sample_effect rate dist tk_effect x = do
  run_tk_effects rate $ tk_effect x
  s <- register_dist_sample (dist_name dist)
  density_terms <- make_edges s $ annotated_densities dist x
  sequence_ [register_prior s term | term <- density_terms]
  register_out_edge s x
  return ()



foreign import bpcall "MCMC:" getInterchangeableId :: IO Int

foreign import bpcall "MCMC:" interchange_entries :: Int -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" register_interchangeable :: Int -> a -> Effect

foreign import bpcall "Modifiables:interchangeable" builtin_interchangeable :: (a->b) -> a -> c -> b

interchangeableIO id x s = let e = builtin_interchangeable unsafePerformIO x s
                           in register_interchangeable id e `seq` e

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
run_lazy' :: Double -> Random a -> IO a
run_lazy' rate (RanBind f g) = do
  x <- unsafeInterleaveIO $ run_lazy' rate f
  run_lazy' rate $ g x
run_lazy' rate (RanDistribution2 dist tk_effect) = do
  x <- modifiableIO $ sampleIO dist
  effect <- sample_effect rate dist tk_effect x
  return (withEffect effect x)
run_lazy' rate (RanDistribution3 dist tk_effect structure do_sample) = do
 -- Note: unsafeInterleaveIO means that we will only execute this line if `value` is accessed.
  value <- unsafeInterleaveIO $ run_lazy do_sample
  return $ structure value (sample_effect rate dist tk_effect)
run_lazy' rate (RanSamplingRate rate2 a) = run_lazy' (rate*rate2) a
run_lazy' rate (Lazy r) = run_lazy' rate r
run_lazy' rate (WithTKEffect action tk_effect) = unsafeInterleaveIO $ do
  result <- unsafeInterleaveIO $ run_lazy' rate action
  effect <- unsafeInterleaveIO $ run_tk_effects rate $ tk_effect result
  return (withEffect effect result)
run_lazy' rate (RanInterchangeable r) = do
  id <- unsafeInterleaveIO $ getInterchangeableId
  register_transition_kernel rate $ interchange_entries id
  return $ liftIO $ IO (\s -> (s, interchangeableIO id (run_lazy' rate r) s))
run_lazy' rate (RanOp op) = op (run_lazy' rate)

mcmc = run_strict' 1.0

makeMCMCModel m = makeModel $ run_strict' 1.0 m

foreign import bpcall "MCMC:" createContext :: [(Key,JSON)] -> CJSON -> IO ContextIndex
makeModel m = createContext prog log where
    prog = (unsafePerformIO m)
    log = c_json $ log_to_json $ prog

foreign import bpcall "MCMC:" writeTraceGraph :: ContextIndex -> IO ()

-- Loggers: we can only log things with the ToJSON property
infix 1 %=%, %>%
name %=% value = (toJSONKey name, toJSON value)
prefix %>% subvalue = (toJSONKey $ prefix ++ "/", log_to_json subvalue)

log_to_json loggers = J.Object $ loggers

-- Define some helper functions
make_densities density x = return [density x]
make_densities' densities x = return $ densities x
pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balanced_product xs = foldt (*) 1 xs

-- maybe I should rename this to (modifiable_list_n n f value) or something.
mapn n f xs = go 0 where
    go i | i==n      = []
         | otherwise = f (xs!!i):go (i+1)

