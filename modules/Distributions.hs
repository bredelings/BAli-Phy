module Distributions where

import Range
import Parameters
import MCMC
import Data.JSON as J
import Tree

-- Define the ProbDensity type
data ProbDensity a = ProbDensity (a->Double) (Double->a) (IO a) Range
density (ProbDensity d _ _ _) = d
quantile (ProbDensity _ q _ _) = q
sampler (ProbDensity _ _ s _) = s
distRange (ProbDensity _ _ _ r) = r

-- FIXME: We might need GADTS for
--   Independant :: (Random a, Random b) -> Random (a,b)
--   Observe :: b -> (ProbDensity b) -> Random ()
--   GetAlphabet :: Random b ?
--   SetAlphabet :: b -> (Random a) -> Random a
--   AddMove :: b -> Random ()

-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside ProbDensity...

-- This implements the Random monad by transforming it into the IO monad.
data Random a = Random (IO a)
              | RandomStructure a (IO a)
              | Sample (ProbDensity a)
              | Observe (ProbDensity b) b
              | AddMove (Int->a)
              | Print b
              | SamplingRate Double (Random a)
              | GetAlphabet
              | SetAlphabet b (Random a)
              | Lazy (Random a)
              | Strict (Random a)


sample dist = Sample dist
observe = Observe

log_all loggers = (Nothing,loggers)

x %% y = (y,(Just x,[]))

maybe_lazy lazy x = if lazy then unsafeInterleaveIO x else x

run_random alpha lazy (Random a) = maybe_lazy lazy a
run_random alpha lazy (IOAndPass f g) = do
  x <- maybe_lazy lazy $ run_random alpha lazy f
  run_random alpha lazy $ g x
run_random alpha lazy (IOReturn v) = return v
run_random alpha lazy (Sample (ProbDensity _ _ (RandomStructure _ a) _)) = run_random alpha lazy a
run_random alpha lazy (Sample (ProbDensity _ _ a _)) = run_random alpha lazy a
run_random alpha lazy GetAlphabet = return alpha
run_random alpha lazy (SetAlphabet a2 x) = run_random a2 x lazy
run_random alpha lazy (AddMove m) = return ()
run_random alpha lazy (SamplingRate _ model) = run_random alpha lazy model
run_random alpha lazy (MFix f) = MFix ((run_random alpha lazy).f)
run_random alpha lazy (Print s) = putStrLn (show s)
run_random alpha lazy (Lazy r) = run_random alpha True r
run_random alpha lazy (Strict r) = run_random alpha False r

run_random' alpha rate lazy (IOAndPass f g) = do
  x <- maybe_lazy lazy $ run_random' alpha rate lazy f
  run_random' alpha rate lazy $ g x
run_random' alpha rate lazy (IOReturn v) = return v
-- It seems like we wouldn't need laziness for `do {x <- r;return x}`.  Do we need it for `r`?
run_random' alpha rate lazy (Sample (ProbDensity pr _ (Random do_sample) range)) = maybe_lazy lazy $ do
  value <- do_sample
  let x = modifiable value
  return (random_variable x (pr x) range rate)
run_random' alpha rate lazy (Sample (ProbDensity pr _ (RandomStructure structure do_sample) range)) = maybe_lazy lazy $ do
  -- we need some mcmc moves here, for crp and for trees
  value <- run_random alpha lazy do_sample
  let x = structure value
  return (random_variable x (pr x) range rate)
run_random' alpha rate lazy (Sample (ProbDensity _ _ s _)) = maybe_lazy lazy $ run_random' alpha rate lazy s
run_random' alpha rate lazy (Observe dist datum) = register_likelihood (density dist datum)
run_random' alpha rate lazy (AddMove m) = register_transition_kernel m
run_random' alpha rate lazy (Print s) = putStrLn (show s)
run_random' alpha rate lazy (MFix f) = MFix ((run_random' alpha rate lazy).f)
run_random' alpha rate lazy (SamplingRate rate2 a) = run_random' alpha (rate*rate2) lazy a
run_random' alpha _    _     GetAlphabet = return alpha
run_random' alpha rate lazy (SetAlphabet a2 x) = run_random' a2 rate lazy x
run_random' alpha rate lazy (Lazy r) = run_random' alpha rate True r
run_random' alpha rate lazy (Strict r) = run_random' alpha rate False r

set_alphabet a x = do (a',_) <- a
                      SetAlphabet a' x
                                                 
gen_model_with_alphabet a m = run_random' a 1.0 False m

gen_model_no_alphabet m = gen_model_with_alphabet (error "No default alphabet!") m

add_logger old name (value,[]) False = old
add_logger old name (value,loggers) do_log = (name,(if do_log then Just value else Nothing, loggers)):old

create_logger name (Just x,  loggers) = do add_parameter name x
                                           create_sub_loggers name loggers
create_logger prefix (Nothing, loggers) = create_sub_loggers prefix loggers
create_logger_with_prefix prefix (name,y) = create_logger (prefix++"/"++name) y
create_sub_loggers prefix loggers = mapM_ (create_logger_with_prefix prefix) loggers

do_log prefix model = do
      result <- model
      create_sub_loggers prefix (snd result)
      return (fst result)

-- Add function to create JSON from logger
log_to_json_one (name,(Nothing,[])) = []
log_to_json_one (name,(Just x,[])) = [(name, J.Object[("value", to_json x)])]
log_to_json_one (name,(Just x,sub_loggers)) = [(name, J.Object[("value", to_json x),("children", log_to_json sub_loggers)])]
log_to_json_one (name,(Nothing,sub_loggers)) = [(name, J.Object[("children", log_to_json sub_loggers)])]

log_to_json loggers = J.Object $ concatMap log_to_json_one loggers

-- Define some helper functions
no_quantile name = error ("Distribution '"++name++"' has no quantile function")

-- Define some basic distributions
builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 3 "sample_shifted_gamma" "Distribution"
sample_shifted_gamma a b shift = Random (IOAction3 builtin_sample_shifted_gamma a b shift)
shifted_gamma a b shift = ProbDensity (shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (above shift)
gamma a b = shifted_gamma a b 0.0

builtin beta_density 3 "beta_density" "Distribution"
builtin beta_quantile 3 "beta_quantile" "Distribution"
builtin builtin_sample_beta 2 "sample_beta" "Distribution"
sample_beta a b = Random (IOAction2 builtin_sample_beta a b)
beta a b = ProbDensity (beta_density a b) (beta_quantile a b) (sample_beta a b) (between 0.0 1.0)

builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 2 "sample_normal" "Distribution"
sample_normal m s = Random (IOAction2 builtin_sample_normal m s)
normal m s = ProbDensity (normal_density m s) (normal_quantile m s) (sample_normal m s) realLine

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin builtin_sample_cauchy 2 "sample_cauchy" "Distribution"
sample_cauchy m s = Random (IOAction2 builtin_sample_cauchy m s)
cauchy m s = ProbDensity (cauchy_density m s) () (sample_cauchy m s) realLine

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution"
sample_laplace m s = Random (IOAction2 builtin_sample_laplace m s)
laplace m s = ProbDensity (laplace_density m s) () (sample_laplace m s) realLine

builtin uniform_density 3 "uniform_density" "Distribution"
builtin builtin_sample_uniform 2 "sample_uniform" "Distribution"
sample_uniform l u = Random (IOAction2 builtin_sample_uniform l u)
uniform l u = ProbDensity (uniform_density l u) () (sample_uniform l u) (between l u)

builtin uniform_int_density 3 "uniform_int_density" "Distribution"
builtin builtin_sample_uniform_int 2 "sample_uniform_int" "Distribution"
sample_uniform_int l u = Random (IOAction2 builtin_sample_uniform_int l u)
uniform_int l u = ProbDensity (uniform_int_density l u) () (sample_uniform_int l u) (integer_between l u)

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution"
dirichlet_density ns ps = builtin_dirichlet_density (list_to_vector ns) (list_to_vector ps)
sample_dirichlet ns = SamplingRate (1.0/sqrt(intToDouble $ length ns)) $ do vs <- mapM (\a-> sample $ gamma a 1.0) ns
                                                                            return $ map (/(sum vs)) vs
dirichlet ns = ProbDensity (dirichlet_density ns) (no_quantile "dirichlet") (sample_dirichlet ns) (Simplex (length ns) 1.0)

dirichlet' l n = dirichlet (replicate l n)

sample_dirichlet_on xs ns = do ps <- sample_dirichlet ns
                               return $ zip xs ps

dirichlet_on_density ns xps = dirichlet_density ns ps where
    ps = map (\(x,p) -> p) xps
dirichlet_on xs ns = ProbDensity (dirichlet_on_density ns) (no_quantile "dirichlet_on") (sample_dirichlet_on xs ns) (LabelledSimplex xs 1.0)
dirichlet_on' xs n = dirichlet_on xs (replicate (length xs) n)

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution"
sample_binomial n p = Random (IOAction2 builtin_sample_binomial n p)
binomial n p = ProbDensity (binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n)

-- A geometric distribution on [0,\infty).  How many failures before a success?
builtin geometric_density 3 "geometric_density" "Distribution"
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution"
sample_geometric p_success = Random (IOAction1 builtin_sample_geometric p_success)
geometric2 p_fail p_success = ProbDensity (geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution"
sample_poisson mu = Random (IOAction1 builtin_sample_poisson mu)
poisson mu = ProbDensity (poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)

builtin builtin_sample_bernoulli 1 "sample_bernoulli" "Distribution"
sample_bernoulli p = Random (IOAction1 builtin_sample_bernoulli p)
bernoulli_density2 p q 1 = (doubleToLogDouble p)
bernoulli_density2 p q 0 = (doubleToLogDouble q)
bernoulli2 p q = ProbDensity (bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1)

bernoulli p = bernoulli2 p (1.0-p)
rbernoulli q = bernoulli2 (1.0-q) q

shifted_exponential mu shift = shifted_gamma 1.0 mu shift
exponential mu = shifted_exponential mu 0.0

normalize v = map (/total) v where total=sum v

do_crp alpha n d = do_crp'' alpha n bins (replicate bins 0) where bins=n+d
do_crp'' alpha 0 bins counts = return []
do_crp'' alpha n bins counts = let inc (c:cs) 0 = (c+1:cs)
                                   inc (c:cs) i = c:(inc cs (i-1))
                                   p alpha counts = normalize (map f counts)
                                   nzeros = length (filter (==0) counts)
                                   f 0 = alpha/(intToDouble nzeros)
                                   f i = intToDouble i
                               in 
                               do c <- sample $ categorical (p alpha counts)
                                  cs <- do_crp'' alpha (n-1) bins (inc counts c) 
                                  return (c:cs)

builtin crp_density 4 "CRP_density" "Distribution"
builtin sample_crp_vector 3 "sample_CRP" "Distribution"
sample_crp alpha n d = Random $ do v <- (IOAction3 sample_crp_vector alpha n d)
                                   return $ list_from_vector v
--crp alpha n d = ProbDensity (crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1))
modifiable_list = map modifiable
crp alpha n d = ProbDensity (crp_density alpha n d) (no_quantile "crp") (RandomStructure modifiable_list $ sample_crp alpha n d) (ListRange $ replicate n subrange)
                  where subrange = integer_between 0 (n+d-1)

mixtureRange ((_,dist1):_) = distRange dist1
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x)
mixture_density [] _ = (doubleToLogDouble 0.0)
sample_mixture ((p1,dist1):l) = dist1
mixture args = ProbDensity (mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args)

builtin builtin_sample_categorical 1 "sample_categorical" "Distribution"
sample_categorical ps = Random (IOAction1 builtin_sample_categorical ps)
categorical ps = ProbDensity (qs!) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where qs = listArray' $ map doubleToLogDouble ps

xrange start end | start < end = start:xrange (start+1) end
                 | otherwise   = []

pick_index 0 (h:t) = (h,t)
pick_index 0 [] = error "Trying to pick from empty list!"
pick_index i (h:t) = let (x, t2) = pick_index (i-1) t
                     in (x, h:t2)

remove_one [] = error "Cannot remove one from empty list"
remove_one list = do i <- sample $ uniform_int 0 (length list-1)
                     return $ pick_index i list

random_tree_edges [l1,l2] _      = return [(l1,l2)]
random_tree_edges leaves internal = do (l1,leaves')  <- remove_one leaves
                                       (l2,leaves'') <- remove_one leaves'
                                       (i,internal') <- remove_one internal
                                       other_edges <- random_tree_edges (i:leaves'') internal'
                                       return $ [(l1,i),(l2,i)]++other_edges

-- If we could stop assuming that leaf branches have names 0..n then this would work
random_tree n = do let num_nodes = 2*n-2
                   edges <- random_tree_edges [0..n-1] [n..num_nodes-1]
                   return $ tree_from_edges num_nodes edges

modifiable_tree tree = Tree (listArray' nodes) (listArray' branches) (numNodes tree) (numBranches tree) where
    nodes =    [ map modifiable (edgesOutOfNode tree n) | n <- xrange 0 (numNodes tree) ]
    branches = [ (modifiable s, modifiable i, modifiable t, modifiable r) | b <- xrange 0 (numBranches tree * 2), let (s,i,t,r) = nodesForEdge tree b]

uniform_topology n = ProbDensity (const $ doubleToLogDouble 1.0) (no_quantile "uniform_topology") (RandomStructure modifiable_tree $ random_tree n) (TreeRange n)

-- define the list distribution
pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balanced_product xs = foldt (*) (doubleToLogDouble 1.0) xs

list_density ds xs = if (length ds == length xs) then pr else (doubleToLogDouble 0.0)
  where densities = zipWith density ds xs
        pr = balanced_product densities

list dists = ProbDensity (list_density dists) (no_quantile "list") do_sample (ListRange (map distRange dists))
             where do_sample = SamplingRate (1.0/sqrt (intToDouble $ length dists)) $ mapM sample dists

-- define different examples of list distributions
iid n dist = list (replicate n dist)

plate n dist_f = list $ map dist_f [0..n-1]
  
-- This contains functions for working with DiscreteDistribution

fmap1 f [] = []
fmap1 f ((x,y):l) = (f x,y):(fmap1 f l)

fmap2 f [] = []
fmap2 f ((x,y):l) = (x,f y):(fmap2 f l)

uniformQuantiles q n = map (\i -> q ((2.0*(intToDouble i)+1.0)/(intToDouble n)) ) (take n [1..])

mix fs ds = [(p*f, x) | (f, d) <- zip' fs ds, (p, x) <- d]

certainly x = [(1.0, x)]

extendDiscreteDistribution d p x = mix [p, 1.0-p] [certainly x, d]

average l = foldl' (\x y->(x+(fst y)*(snd y))) 0.0 l

uniformGrid n = [( 1.0/n', (2.0*i'+1.0)/(2.0*n') ) | i <- take n [0..], let n' = intToDouble n, let i'=intToDouble i]

uniformDiscretize dist n = fmap2 (quantile dist) (uniformGrid n)

-- This contains exp-transformed functions
expTransform (ProbDensity d q s r) = ProbDensity pdf' q' s' r' 
 where 
  pdf' x = (d $ log x)/(doubleToLogDouble x)
  q'   = exp . q
  s'   = do v <- sample $ ProbDensity d q s r
            return $ exp v
  r'   = expTransformRange r
  
log_normal mu sigma = expTransform $ normal mu sigma
log_exponential mu = expTransform $  exponential mu
log_gamma a b = expTransform $ gamma a b
log_laplace m s = expTransform $ laplace m s
log_cauchy m s = expTransform $ cauchy m s

safe_exp x = if (x < (-20.0)) then
               exp (-20.0)
             else if (x > 20.0) then
               exp 20.0
             else
               exp x

dpm n alpha mean_dist noise_dist= do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist
  sigmaOverMu <- sample $ iid (n+delta) noise_dist

  category <- sample $ crp alpha n delta

--  Log "dpm:n_categories" (length (nub category))

  z <- sample $ iid n (normal 0.0 1.0)

  AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])

  return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let k=category!!i]

dp n alpha mean_dist = do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist

  category <- sample $ crp alpha n delta
--  Log "dp:n_categories" (length (nub category))

  AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])

  return [ mean!!k | i <- take n [0..], let k=category!!i]

