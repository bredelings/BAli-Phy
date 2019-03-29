module Probability where

import Range
import Parameters
import MCMC
import Data.JSON as J
import Tree

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
data Random a = RandomStructure a (Random a)
              | RandomStructureAndPDF a (Random a)
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

log_all loggers = (Nothing,loggers)

x %% y = (y,(Just x,[]))

run_strict alpha (IOAndPass f g) = do
  x <- run_strict alpha f
  run_strict alpha $ g x
run_strict alpha (IOReturn v) = return v
run_strict alpha (LiftIO a) = a
run_strict alpha GetAlphabet = return alpha
run_strict alpha (SetAlphabet a2 x) = run_strict a2 x
run_strict alpha (AddMove m) = return ()
run_strict alpha (Print s) = putStrLn (show s)
run_strict alpha (Lazy r) = run_lazy alpha r

run_lazy alpha (RandomStructure _ a) = run_lazy alpha a
run_lazy alpha (RandomStructureAndPDF _ a) = run_lazy alpha a
run_lazy alpha (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy alpha f
  run_lazy alpha $ g x
run_lazy alpha (IOReturn v) = return v
run_lazy alpha (LiftIO a) = a
run_lazy alpha (Sample (Distribution _ _ a _)) = run_lazy alpha a
run_lazy alpha (SampleWithInitialValue (Distribution _ _ a _) _) = run_lazy alpha a
run_lazy alpha GetAlphabet = return alpha
run_lazy alpha (SetAlphabet a2 x) = run_lazy a2 x
run_lazy alpha (SamplingRate _ model) = run_lazy alpha model
run_lazy alpha (MFix f) = MFix ((run_lazy alpha).f)
run_lazy alpha (Strict r) = run_strict alpha r


-- Question: why do we need to duplicate things over RandomStructure and Random?
--           why do we have 5 different versions of Sample?
--           It seems like if we moved Random and RandomStructure up to the top level this would solve some things.  
--              But what would that mean?
--
-- Isn't Sample a special case of SampleWithInitialValue?
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
run_strict' alpha rate (AddMove m) = register_transition_kernel m
run_strict' alpha rate (Print s) = putStrLn (show s)
run_strict' alpha rate (Lazy r) = run_lazy' alpha rate r

run_lazy' alpha rate (LiftIO a) = a
run_lazy' alpha rate (IOAndPass f g) = do
  x <- unsafeInterleaveIO $ run_lazy' alpha rate f
  run_lazy' alpha rate $ g x
run_lazy' alpha rate (IOReturn v) = return v
-- It seems like we wouldn't need laziness for `do {x <- r;return x}`.  Do we need it for `r`?
run_lazy' alpha rate (Sample dist@(Distribution _ _ (RandomStructure structure do_sample) range)) = do
  -- we need some mcmc moves here, for crp and for trees
  value <- run_lazy alpha do_sample
  let x = structure value
  return $ random_variable x (density dist x) range rate
run_lazy' alpha rate (Sample dist@(Distribution _ _ (RandomStructureAndPDF structure_and_pdf do_sample) range)) = do
  -- we need some mcmc moves here, for crp and for trees
  value <- run_lazy alpha do_sample
  let (x,pdf) = structure_and_pdf value rv
      rv = random_variable x pdf range rate
  return x
run_lazy' alpha rate (SampleWithInitialValue dist@(Distribution _ _ (RandomStructure structure do_sample) range) initial_value) = do
  -- maybe we need to perform the sample and not use the result, in order to force the parameters of the distribution? 
  -- we need some mcmc moves here, for crp and for trees
  let x = structure initial_value
  return $ random_variable x (density dist x) range rate
run_lazy' alpha rate (SampleWithInitialValue dist@(Distribution _ _ (RandomStructureAndPDF structure_and_pdf do_sample) range) initial_value) = do
  -- maybe we need to perform the sample and not use the result, in order to force the parameters of the distribution? 
  -- we need some mcmc moves here, for crp and for trees
  let (x,pdf) = structure_and_pdf initial_value rv
      rv = random_variable x pdf range rate
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
make_densities density = \x -> [density x]

-- Define some basic distributions
builtin shifted_gamma_density 4 "shifted_gamma_density" "Distribution"
builtin shifted_gamma_quantile 4 "shifted_gamma_quantile" "Distribution"
builtin builtin_sample_shifted_gamma 3 "sample_shifted_gamma" "Distribution"
sample_shifted_gamma a b shift = RandomStructure modifiable $ liftIO (IOAction3 builtin_sample_shifted_gamma a b shift)
shifted_gamma a b shift = Distribution (make_densities $ shifted_gamma_density a b shift) (shifted_gamma_quantile a b shift) (sample_shifted_gamma a b shift) (above shift)
gamma a b = shifted_gamma a b 0.0

builtin beta_density 3 "beta_density" "Distribution"
builtin beta_quantile 3 "beta_quantile" "Distribution"
builtin builtin_sample_beta 2 "sample_beta" "Distribution"
sample_beta a b = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_beta a b)
beta a b = Distribution (make_densities $ beta_density a b) (beta_quantile a b) (sample_beta a b) (between 0.0 1.0)

builtin normal_density 3 "normal_density" "Distribution"
builtin normal_quantile 3 "normal_quantile" "Distribution"
builtin builtin_sample_normal 2 "sample_normal" "Distribution"
sample_normal m s = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_normal m s)
normal m s = Distribution (make_densities $ normal_density m s) (normal_quantile m s) (sample_normal m s) realLine

builtin cauchy_density 3 "cauchy_density" "Distribution"
builtin builtin_sample_cauchy 2 "sample_cauchy" "Distribution"
sample_cauchy m s = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_cauchy m s)
cauchy m s = Distribution (make_densities $ cauchy_density m s) () (sample_cauchy m s) realLine

builtin laplace_density 3 "laplace_density" "Distribution"
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution"
sample_laplace m s = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_laplace m s)
laplace m s = Distribution (make_densities $ laplace_density m s) () (sample_laplace m s) realLine

builtin uniform_density 3 "uniform_density" "Distribution"
builtin builtin_sample_uniform 2 "sample_uniform" "Distribution"
sample_uniform l u = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_uniform l u)
uniform l u = Distribution (make_densities $ uniform_density l u) () (sample_uniform l u) (between l u)

builtin uniform_int_density 3 "uniform_int_density" "Distribution"
builtin builtin_sample_uniform_int 2 "sample_uniform_int" "Distribution"
sample_uniform_int l u = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_uniform_int l u)
uniform_int l u = Distribution (make_densities $ uniform_int_density l u) () (sample_uniform_int l u) (integer_between l u)

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution"
dirichlet_density ns ps = builtin_dirichlet_density (list_to_vector ns) (list_to_vector ps)
sample_dirichlet ns = SamplingRate (1.0/sqrt(intToDouble $ length ns)) $ do vs <- mapM (\a-> sample $ gamma a 1.0) ns
                                                                            return $ map (/(sum vs)) vs
dirichlet ns = Distribution (make_densities $ dirichlet_density ns) (no_quantile "dirichlet") (sample_dirichlet ns) (Simplex (length ns) 1.0)

dirichlet' l n = dirichlet (replicate l n)

sample_dirichlet_on xs ns = do ps <- sample_dirichlet ns
                               return $ zip xs ps

dirichlet_on_density ns xps = dirichlet_density ns ps where
    ps = map (\(x,p) -> p) xps
dirichlet_on xs ns = Distribution (make_densities $ dirichlet_on_density ns) (no_quantile "dirichlet_on") (sample_dirichlet_on xs ns) (LabelledSimplex xs 1.0)
dirichlet_on' xs n = dirichlet_on xs (replicate (length xs) n)

builtin binomial_density 3 "binomial_density" "Distribution"
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution"
sample_binomial n p = RandomStructure modifiable $ liftIO (IOAction2 builtin_sample_binomial n p)
binomial n p = Distribution (make_densities $ binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n)

-- A geometric distribution on [0,\infty).  How many failures before a success?
builtin geometric_density 3 "geometric_density" "Distribution"
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution"
sample_geometric p_success = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_geometric p_success)
geometric2 p_fail p_success = Distribution (make_densities $ geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0)

geometric p = geometric2 (1.0-p) p
rgeometric q = geometric2 q (1.0-q)

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution"
sample_poisson mu = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_poisson mu)
poisson mu = Distribution (make_densities $ poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)

builtin builtin_sample_bernoulli 1 "sample_bernoulli" "Distribution"
sample_bernoulli p = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_bernoulli p)
bernoulli_density2 p q 1 = (doubleToLogDouble p)
bernoulli_density2 p q 0 = (doubleToLogDouble q)
bernoulli2 p q = Distribution (make_densities $ bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1)

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
sample_crp alpha n d = RandomStructure modifiable $ liftIO $ do v <- (IOAction3 sample_crp_vector alpha n d)
                                                                return $ list_from_vector v
--crp alpha n d = Distribution (crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1))
modifiable_list = map modifiable
crp alpha n d = Distribution (make_densities $ crp_density alpha n d) (no_quantile "crp") (RandomStructure modifiable_list $ sample_crp alpha n d) (ListRange $ replicate n subrange)
                  where subrange = integer_between 0 (n+d-1)

mixtureRange ((_,dist1):_) = distRange dist1
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x)
mixture_density [] _ = (doubleToLogDouble 0.0)
sample_mixture ((p1,dist1):l) = dist1
mixture args = Distribution (make_densities $ mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args)

builtin builtin_sample_categorical 1 "sample_categorical" "Distribution"
sample_categorical ps = RandomStructure modifiable $ liftIO (IOAction1 builtin_sample_categorical ps)
categorical ps = Distribution (make_densities $ qs!) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
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

random_tree_edges [l1] _         = return []
random_tree_edges [l1,l2] _      = return [(l1,l2)]
random_tree_edges leaves internal = do (l1,leaves')  <- remove_one leaves
                                       (l2,leaves'') <- remove_one leaves'
                                       let (i:internal') = internal
                                       other_edges <- random_tree_edges (i:leaves'') internal'
                                       return $ [(l1,i),(l2,i)]++other_edges

random_tree 1 = return $ Tree (listArray' [[]]) (listArray' []) 1 0
random_tree n = do let num_nodes = 2*n-2
                   edges <- random_tree_edges [0..n-1] [n..num_nodes-1]
                   -- This flipping is suppose flip edges from (internal,leaf) -> (leaf, internal)
                   let maybe_flip (x,y) | (y<x)     = (y,x)
                                        | otherwise = (x,y)
                   -- Then the sorting is supposed order edges like (0,_), (1,_), (2,_)
                   -- in order to assign leaf branches the names 0..n-1
                   let sorted_edges = quicksortWith (\(leaf,internal) -> leaf) $ map maybe_flip edges
                   return $ tree_from_edges num_nodes sorted_edges

modifiable_tree mod tree = Tree (listArray' nodes) (listArray' branches) (numNodes tree) (numBranches tree) where
    nodes =    [ map mod (edgesOutOfNode tree n) | n <- xrange 0 (numNodes tree) ]
    branches = [ (mod s, mod i, mod t, mod r) | b <- xrange 0 (numBranches tree * 2), let (s,i,t,r) = nodesForEdge tree b]


uniform_topology_pr 1 = doubleToLogDouble 1.0
uniform_topology_pr 2 = doubleToLogDouble 1.0
uniform_topology_pr n = uniform_topology_pr (n-1) / (doubleToLogDouble $ intToDouble $ 2*n-5)

modifiable_tree_pdf n value rv = let mod v = rv `seq` modifiable v
                                     tree = modifiable_tree mod value
                                 in (tree, uniform_topology_pr n)

uniform_topology n = Distribution (\tree-> uniform_topology_pr n) (no_quantile "uniform_topology") (RandomStructureAndPDF (modifiable_tree_pdf n) (random_tree n)) (TreeRange n)


-- define the list distribution
pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balanced_product xs = foldt (*) (doubleToLogDouble 1.0) xs

list_densities (d:ds) (x:xs) = densities d x ++ list_densities ds xs
list_densities [] []         = []
list_densities _  _          = [doubleToLogDouble 0.0]

list dists = Distribution (list_densities dists) (no_quantile "list") do_sample (ListRange (map distRange dists))
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
expTransform (Distribution d q s r) = Distribution pdf' q' s' r' 
 where 
  pdf' x = case (d $ log x) of [pdf] -> pdf/(doubleToLogDouble x)
  q'   = exp . q
  s'   = do v <- sample $ Distribution d q s r
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

  z <- sample $ iid n (normal 0.0 1.0)

  category <- Strict $ do category <- Lazy $ sample $ crp alpha n delta
                          AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                          return category

  return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let k=category!!i]

dp n alpha mean_dist = do 

  let delta = 4

  mean <- sample $ iid (n+delta) mean_dist

  category <- Strict $ do category <- Lazy $ sample $ crp alpha n delta
                          AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1])
                          return category

  return [ mean!!k | i <- take n [0..], let k=category!!i]

