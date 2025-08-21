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

import Data.Semigroup
import Data.Monoid
import Data.JSON.Encoding

-------------------------- AnnotatedDensity ------------------------

data AnnotatedDensity a where
    InEdge :: String -> b -> AnnotatedDensity ()
    ADBind :: AnnotatedDensity b -> (b -> AnnotatedDensity a) -> AnnotatedDensity a
    ADReturn :: a -> AnnotatedDensity a
    ProbFactor :: Double -> AnnotatedDensity ()

in_edge name node = InEdge name node


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


makeEdges :: Effect -> AnnotatedDensity a -> IO a
makeEdges event (ADReturn x) = return x
makeEdges event (ADBind f g) = do x <- makeEdges event f
                                  makeEdges event (g x)
makeEdges event (InEdge name node) = do register_in_edge node event name
                                        return ()

-- We can observe from these.
-- PROBLEM: What if observing from a distribution is supposed to update
--          a summary statistic?  Can we then register a properties object for each
--          observation?
class Dist d => HasAnnotatedPdf d where
    type DistProperties d :: Type
    type DistProperties d = ()
    annotated_densities :: d -> Result d -> AnnotatedDensity ([LogDouble], DistProperties d)

densities dist x = fst $ get_densities $ annotated_densities dist x
density dist x = balancedProduct (densities dist x)


---------------------------- TKEffects --------------------------

data TKEffects a = SamplingRate Double (TKEffects a)
                 | TKLiftIO (Double -> IO a)
                 | TKReturn a
                 | forall b . TKBind (TKEffects b) (b -> TKEffects a)

instance MonadIO TKEffects where
    liftIO io = TKLiftIO (\_ -> io)

instance Functor TKEffects where
    fmap f x = TKBind x (\result -> TKReturn (f result))

instance Applicative TKEffects where
    pure  x = TKReturn x
    f <*> x = TKBind x (\x' -> TKBind f (\f' -> pure (f' x')))

instance Monad TKEffects where
    return x = TKReturn x
    f >>= g  = TKBind f g

--------------------------- Random a ----------------------------

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



instance Dist (Random a) where
    type Result (Random a) = a

instance IOSampleable (Random a) where
    sampleIO dist = runRandomLazy dist

--------------------------- class Sampleable -----------------------------

-- These objects have a corresponding Random action.
class Dist d => Sampleable d where
    sample :: d -> Random (Result d)

prior = sample

instance Sampleable (Random a) where
    sample r = r

--------------------------- class SampleableWithProps -------------------

class (Sampleable d, HasAnnotatedPdf d) => SampleableWithProps d where
    sampleWithProps :: d -> Random (Result d, DistProperties d)

-------------------------------------------------------------------------

registerDistProperties event props = register_dist_property event props "properties"

{- Note: sampleEffectProbs is NEARLY the same as observe.

Differences include:
* observe starts with liftIO .. this allows it to run in the Random monad.
* observe uses register_likelihood vs register_prior
* observe user register_dist_observe vs register_dist_sample
* observe does NOT run tk_effects

We could actually factor out the common parts if we could pass a constant SampleType = Sample | Observe

-}

{- This runs as a SEQUENCE of operations in the IO monad.
 - The sequence can in theory be changeable, if it depends (for example) on the number of density terms.
 - It would seem that the coalescent probability, which has a fixed number of terms but gets them by sorting,
   appears to have a changeable list of density terms.
 -}

sampleEffectProps rate dist tk_effect x = do
  runTkEffects rate $ tk_effect x
  s <- register_dist_sample (dist_name dist)
  register_out_edge s x
  (densityTerms, props) <- makeEdges s $ annotated_densities dist x
  registerDistProperties s props
  sequence_ [register_prior s term | term <- densityTerms]
  return props

sampleEffect rate dist tk_effect x = do
  sampleEffectProps rate dist tk_effect x
  return () 

infix 0 `observe`

observe datum dist = liftIO $ do
  s <- register_dist_observe (dist_name dist)
  register_out_edge s datum
  (densityTerms, props) <- makeEdges s $ annotated_densities dist datum
  registerDistProperties s props
  sequence_ [register_likelihood s term | term <- densityTerms]
  return props

possible = 1
impossible = 0
require p = if p then possible else impossible

condition cond = liftIO $ do
  s <- register_dist_observe "condition"
  register_likelihood s (require cond)
  return ()

{- NOTE: Problems with lists of probability factors.

In order to determine whether a PDF term corresponds to a newly sampled variable,
we check if the term has been newly registered.  However, if the list of factors
is itself changeable  (as in the coalescent pdf which does an internal sort), we
can end up registering new terms (I think), which are then ignored in the ratio
(I think).

An alternative would be to look not at the individual terms, but at the
distribution identifier created in `s <- register_dist_*`.  This shouldn't get
invalidated if the PDF terms change.

However, this leaves the question of what to do if the coalescent tree itself
changes dimension.

 -}

instance MonadIO Random where
    liftIO io = RanOp (\interp -> io)

lazy = Lazy
interchangeable = RanInterchangeable
infixl 2 `withTKEffect`
withTKEffect = WithTKEffect

addLogger logger = liftIO $ registerLogger logger

do_nothing _ = return ()

runRandomStrict :: Random a -> IO a
runRandomStrict (RanBind f g) = do
  x <- runRandomStrict f
  runRandomStrict $ g x
runRandomStrict (RanSamplingRate _ a) = runRandomStrict a
runRandomStrict (RanInterchangeable r) = return r
-- These are the lazily executed parts of the strict monad.
runRandomStrict dist@(RanDistribution2 _ _) = runRandomLazy dist
runRandomStrict dist@(RanDistribution3 _ _ _ _) = runRandomLazy dist
runRandomStrict e@(WithTKEffect _ _) = runRandomLazy e
runRandomStrict (Lazy r) = unsafeInterleaveIO $ runRandomLazy r
runRandomStrict (RanInterchangeable _) = error "runRandomStrict: RanInterchangeable"
runRandomStrict (PerformTKEffect _) = error "runRandomStrict: PerformTKEffect"
runRandomStrict (RanOp op) = op runRandomStrict

class Monad m => AddMove m where
    addMove :: Double -> TransitionKernel -> m Effect

instance AddMove Random where
    addMove rate move = RanSamplingRate rate $ PerformTKEffect $ TKLiftIO $ (\rate -> registerTransitionKernel rate move)

add_move m = TKLiftIO $ (\rate -> registerTransitionKernel rate m)

instance AddMove TKEffects where
    addMove rate' move = TKLiftIO $ (\rate -> registerTransitionKernel (rate*rate') move)

runTkEffects :: Double -> TKEffects a -> IO a
runTkEffects rate (TKBind f g) = do x <- runTkEffects rate f
                                    runTkEffects rate $ g x
runTkEffects rate (TKReturn v) = return v
runTkEffects rate (TKLiftIO action) = action rate
runTkEffects rate (SamplingRate rate2 a) = runTkEffects (rate*rate2) a
-- LiftIO and Print are only here for debugging purposes:
--  runTkEffects alpha rate (LiftIO a) = a
--  runTkEffects alpha rate (Print s) = putStrLn (show s)

runRandomLazy :: Random a -> IO a
runRandomLazy (RanBind f g) = do
  x <- unsafeInterleaveIO $ runRandomLazy f
  runRandomLazy $ g x
runRandomLazy (RanSamplingRate _ a) = runRandomLazy a
-- Problem: distributions aren't part of the Random monad!
runRandomLazy (RanDistribution2 dist _) = unsafeInterleaveIO $ sampleIO dist
runRandomLazy (RanDistribution3 _ _ _ do_sample) = unsafeInterleaveIO $ runRandomLazy do_sample
runRandomLazy (PerformTKEffect e) = runTkEffects 1.0 e
runRandomLazy (WithTKEffect action _) = runRandomLazy action
runRandomLazy (Lazy a) = runRandomLazy a
runRandomLazy (RanInterchangeable a) = return a
runRandomLazy (RanOp op) = op runRandomLazy

-- Also, shouldn't the modifiable function actually be some kind of monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

runMCMCStrict :: Double -> Random a -> IO a
runMCMCStrict rate (RanBind f g) = do
  x <- runMCMCStrict rate f
  runMCMCStrict rate $ g x
runMCMCStrict rate (PerformTKEffect e) = runTkEffects rate e
runMCMCStrict rate (RanSamplingRate rate2 a) = runMCMCStrict (rate*rate2) a
-- These are the lazily executed parts of the strict monad.
runMCMCStrict rate ix@(RanInterchangeable r) = runMCMCLazy rate ix
runMCMCStrict rate dist@(RanDistribution2 _ _) = runMCMCLazy rate dist
runMCMCStrict rate dist@(RanDistribution3 _ _ _ _) = runMCMCLazy rate dist
runMCMCStrict rate e@(WithTKEffect _ _) = runMCMCLazy rate e
runMCMCStrict rate (Lazy r) = unsafeInterleaveIO $ runMCMCLazy rate r -- See Note below.
runMCMCStrict rate (RanOp op) = op (runMCMCStrict rate)

{- NOTE: unsafeInterleaveIO $ runMCMCLazy

In order for (runMCMCLazy) to actually be lazy, we need to avoid returning
SOMETHING `seq` result.  And this means that we need to frequently
intersperse unsafeInterleaveIO to avoid `seq`-ing on previous statements.
-}

-- Note on unsafeInterleaveIO:
--       Simply using runRandomLazy does not guarantee that the result of runRandomLazy
--       will not be demanded.  (It guarantees that f >>= g that are INSIDE runRandomLazy
--       won't demand the result of the f).
--       We need to guard any IO operations with unsafeInterleaveIO if we
--       want to prevent their results from being demanded.
--
-- QUESTION: So, do we need to guard the execution of Distributions with unsafeInterleaveIO?
-- ANSWER: No.  If its not the last entry in a sequence, it will get unsafeInterleaveIO from
--         runMCMCLazy _ (IOAndPass _ _).
--         If it is run from runMCMCStrict directly, then it is run with
--         unsafeInterleaveIO $ runMCMCLazy, so we get an unsafeInterleaveIO from there.
--
runMCMCLazy :: Double -> Random a -> IO a
runMCMCLazy rate (RanBind f g) = do
  x <- unsafeInterleaveIO $ runMCMCLazy rate f
  runMCMCLazy rate $ g x
runMCMCLazy rate (RanDistribution2 dist tk_effect) = do
  x <- modifiableIO $ sampleIO dist
  effect <- sampleEffect rate dist tk_effect x
  return (withEffect effect x)
runMCMCLazy rate (RanDistribution3 dist tk_effect structure do_sample) = do
 -- Note: unsafeInterleaveIO means that we will only execute this line if `value` is accessed.
  value <- unsafeInterleaveIO $ runRandomLazy do_sample
  return $ structure value (sampleEffect rate dist tk_effect)
runMCMCLazy rate (RanSamplingRate rate2 a) = runMCMCLazy (rate*rate2) a
runMCMCLazy rate (Lazy r) = runMCMCLazy rate r
runMCMCLazy rate (WithTKEffect action tk_effect) = unsafeInterleaveIO $ do
  result <- unsafeInterleaveIO $ runMCMCLazy rate action
  effect <- unsafeInterleaveIO $ runTkEffects rate $ tk_effect result
  return (withEffect effect result)
runMCMCLazy rate (RanInterchangeable r) = do
  id <- unsafeInterleaveIO $ getInterchangeableId
  registerTransitionKernel rate $ interchangeEntries id
  return $ liftIO $ IO (\s -> (s, interchangeableIO id (runMCMCLazy rate r) s))
runMCMCLazy rate (RanOp op) = op (runMCMCLazy rate)

mcmc = runMCMCStrict 1.0

makeMCMCModel m = makeModel $ runMCMCStrict 1.0 m

foreign import bpcall "MCMC:" createContext :: [(Key,J.Value)] -> CJSON -> IO ContextIndex

makeModel m = createContext prog log where
    prog = (unsafePerformIO m)
    log = toCJSON $ logToJson $ prog

foreign import bpcall "MCMC:" writeTraceGraph :: ContextIndex -> IO ()

-- Loggers: we can only log things with the ToJSON property
infix 1 %=%, %>%
name %=% value = (toJSONKey name, toJSON value)
prefix %>% subvalue = (toJSONKey $ prefix ++ "/", logToJson subvalue)

toSeries :: (ToJSONKey k, ToJSON v) => [(k,v)] -> Series
toSeries pairs = foldr (<>) mempty [toJSONKey k .= v | (k,v) <- pairs]

(.>) :: String -> Series -> Series
prefix .> subvalue = pairStr (prefix ++ "/") (pairs $ subvalue)

logToJson :: (ToJSONKey k, ToJSON v) => [(k,v)] -> Value
logToJson loggers = J.Object $ [(toJSONKey k, toJSON v) | (k,v) <- loggers]

-- Define some helper functions
make_densities density x = return ([density x],())
make_densities' densities x = return (densities x,())
pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balancedProduct xs = foldt (*) 1 xs

-- maybe I should rename this to (modifiableList_n n f value) or something.
mapn n f xs = go 0 where
    go i | i==n      = []
         | otherwise = f (xs!!i):go (i+1)


------------------------ Modifiable structures --------------------------
                       
triggeredModifiableStructure :: ((forall a.a -> a) -> b -> b) -> b -> (b -> IO ()) -> b
triggeredModifiableStructure modStructure value effect = triggered_x
    where raw_x       = modStructure modifiable value
          effect'     = unsafePerformIO $ effect raw_x
          triggered_x = modStructure (withEffect effect') raw_x

applyModifier :: (forall a.a -> a) -> b -> b
applyModifier x y = x y

modifiableStructure :: b -> (b -> IO ()) -> b
modifiableStructure = triggeredModifiableStructure applyModifier

-- It seems like we could return raw_x in most cases, except the case of a tree.
-- But in the tree case, we could return triggered_x.

------------------------- Interchangeables ---------------------------
                       
foreign import bpcall "MCMC:" getInterchangeableId :: IO Int

foreign import bpcall "MCMC:" interchangeEntriesRaw :: Int -> ContextIndex -> IO ()
interchangeEntries id = TransitionKernel $ interchangeEntriesRaw id

foreign import bpcall "MCMC:" registerInterchangeable :: Int -> a -> Effect

foreign import bpcall "Modifiables:interchangeable" interchangeableRaw :: (a->b) -> a -> c -> b

interchangeableIO id x s = let e = interchangeableRaw unsafePerformIO x s
                           in registerInterchangeable id e `seq` e

