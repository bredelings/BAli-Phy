module Distributions where
{
import Range;
import Parameters;
import MCMC;

-- Define the ProbDensity type
data ProbDensity = ProbDensity a b c d;
density (ProbDensity d _ _ _) = d;
quantile (ProbDensity _ q _ _) = q;
sampler (ProbDensity _ _ s _) = s;
distRange (ProbDensity _ _ _ r) = r;

-- This implements the Random monad by transforming it into the IO monad.
data Random a = Random a | NoLog a | Prefix a b | Log a b | Observe a b | AddMove (Int->a);

sample (IOReturn v) = IOReturn v;
sample (IOAndPass f g) = IOAndPass (sample f) (\x -> sample $ g x);
sample (IOAnd f g) = IOAnd (sample f) (sample g);
sample (ProbDensity p q (Random a) r) = a;
sample (ProbDensity p q s r) = sample s;
sample (NoLog a) = sample a;
sample (Prefix _ a) = sample a;
sample (Log _ a) = sample a;
sample (AddMove m) = return ();

sample' ps l (IOReturn v) = IOReturn v;
sample' ps l (IOAndPass f g) = IOAndPass (sample' ps l f) (\x -> sample' ps l $ g x);
sample' ps l (IOAnd f g) = IOAnd (sample' ps l f) (sample' ps l g);
sample' ps l (ProbDensity p q (Random a) r) = do { let {v = unsafePerformIO' a;};
                                              m <- new_random_modifiable r v;
                                              register_probability (p m);
                                              return m };
sample' ps l (ProbDensity p q s r) = sample' ps l s;

sample' ps l (NoLog a) = sample' ps False a;
sample' ps l (Prefix p a) = sample' (p:ps) l a;
sample' ps l (Observe v dist) = register_probability (density dist v);
sample' ps l (AddMove m) = register_transition_kernel m;
sample' ps True (Log name x) = add_parameter (prefix_name ps name) x;
sample' ps False (Log name x) = return ();

add_prefix p m = Prefix p m;
gen_model m = sample' [] True m;

prefix_name ps name = foldl (\a b -> b++"."++a) name ps;
name ~~ dist = do { x <- dist ; Log name x ; return x};
name @@ a = Prefix name a;


-- Define some helper functions
distDefaultValue d = unsafePerformIO' $ sample d;

no_quantile name = error ("Distribution '"++name++"' has no quantile function");

-- Define some basic distributions
builtin gamma_density 3 "gamma_density" "Distribution";
builtin gamma_quantile 3 "gamma_quantile" "Distribution";
builtin builtin_sample_gamma 2 "sample_gamma" "Distribution";
sample_gamma a b = Random (IOAction2 builtin_sample_gamma a b);
gamma a b = ProbDensity (gamma_density a b) (gamma_quantile a b) (sample_gamma a b) (above 0.0);

builtin beta_density 3 "beta_density" "Distribution";
builtin beta_quantile 3 "beta_quantile" "Distribution";
builtin builtin_sample_beta 2 "sample_beta" "Distribution";
sample_beta a b = Random (IOAction2 builtin_sample_beta a b);
beta a b = ProbDensity (beta_density a b) (beta_quantile a b) (sample_beta a b) (between 0.0 1.0);

builtin normal_density 3 "normal_density" "Distribution";
builtin normal_quantile 3 "normal_quantile" "Distribution";
builtin builtin_sample_normal 2 "sample_normal" "Distribution";
sample_normal m s = Random (IOAction2 builtin_sample_normal m s);
normal m s = ProbDensity (normal_density m s) (normal_quantile m s) (sample_normal m s) realLine;

builtin cauchy_density 3 "cauchy_density" "Distribution";
builtin builtin_sample_cauchy 2 "sample_cauchy" "Distribution";
sample_cauchy m s = Random (IOAction2 builtin_sample_cauchy m s);
cauchy m s = ProbDensity (cauchy_density m s) () (sample_cauchy m s) realLine;

builtin laplace_density 3 "laplace_density" "Distribution";
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution";
sample_laplace m s = Random (IOAction2 builtin_sample_laplace m s);
laplace m s = ProbDensity (laplace_density m s) () (sample_laplace m s) realLine;

builtin uniform_density 3 "uniform_density" "Distribution";
builtin builtin_sample_uniform 2 "sample_uniform" "Distribution";
sample_uniform l u = Random (IOAction2 builtin_sample_uniform l u);
uniform l u = ProbDensity (uniform_density l u) () (sample_uniform l u) (between l u);

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution";
dirichlet_density ps xs = builtin_dirichlet_density (listToVectorDouble ps) (listToVectorDouble xs);
sample_dirichlet ps = do { vs <- mapM (\a->gamma a 1.0) ps;
                           return $ map (/(sum vs)) vs};
dirichlet ps = ProbDensity (dirichlet_density ps) (no_quantile "dirichlet") (sample_dirichlet ps) (Simplex (length ps) 1.0);

dirichlet' n x = dirichlet (replicate n x);

builtin binomial_density 3 "binomial_density" "Distribution";
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution";
sample_binomial n p = Random (IOAction2 builtin_sample_binomial n p);
binomial n p = ProbDensity (binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (integer_between 0 n);

-- A geometric distribution on [0,\infty).  How many failures before a success?
builtin geometric_density 3 "geometric_density" "Distribution";
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution";
sample_geometric p_success = Random (IOAction1 builtin_sample_geometric p_success);
geometric2 p_fail p_success = ProbDensity (geometric_density p_fail p_success) (no_quantile "geometric") (sample_geometric p_success) (integer_above 0);

geometric p = geometric2 (1.0-p) p;
rgeometric q = geometric2 q (1.0-q);

builtin poisson_density 2 "poisson_density" "Distribution";
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution";
sample_poisson mu = Random (IOAction1 builtin_sample_poisson mu);
poisson mu = ProbDensity (poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0);

builtin builtin_sample_bernoulli 1 "sample_bernoulli" "Distribution";
-- sample_bernoulli p = Random (IOAction1 builtin_sample_bernoulli p);
sample_bernoulli p = Random (IOReturn 0);
bernoulli_density2 p q 1 = (doubleToLogDouble p);
bernoulli_density2 p q 0 = (doubleToLogDouble q);
bernoulli2 p q = ProbDensity (bernoulli_density2 p q) (no_quantile "bernoulli") (sample_bernoulli p) (integer_between 0 1);

bernoulli p = bernoulli2 p (1.0-p);
rbernoulli q = bernoulli2 (1.0-q) q;

builtin builtin_sample_exponential 1 "sample_exponential" "Distribution";
builtin exponential_density 2 "exponential_density" "Distribution";
exponential_quantile mu p = gamma_quantile 1.0 mu p;
sample_exponential mu = Random (IOAction1 builtin_sample_exponential mu);
exponential mu = ProbDensity (exponential_density mu) (exponential_quantile mu) (sample_exponential mu) (above 0.0);

normalize v = map (/total) v where {total=sum v};

do_crp alpha n d = do_crp'' alpha n bins (replicate bins 0) where {bins=n+d};
do_crp'' alpha 0 bins counts = return [];
do_crp'' alpha n bins counts = let { inc (c:cs) 0 = (c+1:cs);
                                     inc (c:cs) i = c:(inc cs (i-1));
                                     p alpha counts = normalize (map f counts);
                                     nzeros = length (filter (==0) counts);
                                     f 0 = alpha/(intToDouble nzeros);
                                     f i = intToDouble i}
                               in 
                               do { c <- categorical (p alpha counts);
                                    cs <- do_crp'' alpha (n-1) bins (inc counts c); 
                                    return (c:cs)};

builtin crp_density 4 "CRP_density" "Distribution";
builtin sample_crp_vector 3 "sample_CRP" "Distribution";
sample_crp alpha n d = Random $ do { v <- (IOAction3 sample_crp_vector alpha n d); return $ list_from_vector v};
crp alpha n d = ProbDensity (crp_density alpha n d) (no_quantile "crp") (do_crp alpha n d) (ListRange $ replicate n $ integer_between 0 (n+d-1));

mixtureRange ((_,dist1):_) = distRange dist1;
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x);
mixture_density [] _ = (doubleToLogDouble 0.0);
sample_mixture ((p1,dist1):l) = dist1;
mixture args = ProbDensity (mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args);

builtin builtin_sample_categorical 1 "sample_categorical" "Distribution";
sample_categorical ps = Random (IOAction1 builtin_sample_categorical ps);
categorical ps = ProbDensity (qs!) (no_quantile "categorical") (sample_categorical ps) (integer_between 0 (length ps - 1))
                where {qs = listArray' $ map doubleToLogDouble ps};

-- define the list distribution
pairs f (x:y:t) = f x y : pairs f t;
pairs _ t       = t;

foldt f z []  = z;
foldt f _ [x] = x;
foldt f z xs  = foldt f z (pairs f xs);

balanced_product xs = foldt (*) (doubleToLogDouble 1.0) xs;

list_density ds xs = if (length ds == length xs) then pr else (doubleToLogDouble 0.0)
  where {densities = zipWith density ds xs;
         pr = balanced_product densities};

list dists = ProbDensity (list_density dists) (no_quantile "list") (sequence dists) (ListRange (map distRange dists));

-- define different examples of list distributions
iid n dist = list (replicate n dist);

plate n dist_f = list $ map dist_f [0..n-1];
  
-- This contains functions for working with DiscreteDistribution
data DiscreteDistribution a = DiscreteDistribution [(Double,a)];

fmap1 f [] = [];
fmap1 f ((x,y):l) = (f x,y):(fmap1 f l);
fmap1 f (DiscreteDistribution l) = DiscreteDistribution (fmap1 f l);

fmap2 f [] = [];
fmap2 f ((x,y):l) = (x,f y):(fmap2 f l);
fmap2 f (DiscreteDistribution l) = DiscreteDistribution (fmap2 f l);

extendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution ((p,x):(fmap1 (\q->q*(1.0-p)) d));

uniformQuantiles q n = map (\i -> q ((2.0*(intToDouble i)+1.0)/(intToDouble n)) ) (take n [1..]);

unwrapDD (DiscreteDistribution l) = l;

mixDiscreteDistributions' (h:t) (h2:t2) = (fmap1 (\q->q*h) h2)++(mixDiscreteDistributions' t t2);
mixDiscreteDistributions' [] [] = [];

mixDiscreteDistributions l1 l2 = DiscreteDistribution (mixDiscreteDistributions' l1 (fmap unwrapDD l2));

average (DiscreteDistribution l) = foldl' (\x y->(x+(fst y)*(snd y))) 0.0 l;

uniformGrid n = DiscreteDistribution [( 1.0/n', (2.0*i'+1.0)/(2.0*n') ) | i <- take n [0..], let {n' = intToDouble n;i'=intToDouble i}];

uniformDiscretize q n = fmap2 q (uniformGrid n);

-- This contains exp-transformed functions
expTransform (ProbDensity d q s r) = ProbDensity pdf' q' s' r' 
 where {
  pdf' x = (d $ log x)/(doubleToLogDouble x);
  q'   = exp . q;
  s'   = do {v <- (ProbDensity d q s r); return $ exp v};
  r'   = Range.expTransform r
 };
  
logNormal mu sigma = expTransform $ normal mu sigma;
logExponential mu = expTransform $  exponential mu;
logGamma a b = expTransform $ gamma a b;
logLaplace m s = expTransform $ laplace m s;
logCauchy m s = expTransform $ cauchy m s;

safe_exp x = if (x < (-20.0)) then
               exp (-20.0);
             else if (x > 20.0) then
               exp 20.0;
             else
               exp x;

dpm n alpha mean_dist noise_dist= Prefix "DPM" $ do 
{
    let {delta = 4};

    mean <- iid (n+delta) mean_dist;
    sigmaOverMu <- iid (n+delta) noise_dist;

    category <- crp alpha n delta;
    Log "category" category;
    Log "n_categories" (length (nub category));

    z <- iid n (normal 0.0 1.0);

    AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1]);

    return [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n [0..], let {k=category!!i}];
};

dp n alpha mean_dist = Prefix "DP" $ do 
{
    let {delta = 4};

    mean <- iid (n+delta) mean_dist;

    category <- crp alpha n delta;
    Log "category" category;
    Log "n_categories" (length (nub category));

    AddMove (\c -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n+delta) c) [0..n-1]);

    return [ mean!!k | i <- take n [0..], let {k=category!!i}];
};

}
