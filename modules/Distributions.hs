module Distributions where
{
import Range;

-- Define the ProbDensity type
data ProbDensity = ProbDensity a b c d;
density (ProbDensity d _ _ _) = d;
quantile (ProbDensity _ q _ _) = q;
sampler (ProbDensity _ _ s _) = s;
distRange (ProbDensity _ _ _ r) = r;

-- This implements the Random monad by transforming it into the IO monad.
data Random a = Random a;

sample (IOReturn v) = IOReturn v;
sample (IOAndPass f g) = IOAndPass (sample f) (\x -> sample $ g x);
sample (IOAnd f g) = IOAnd (sample f) (sample g);
sample (ProbDensity p q (Random a) r) = a;
sample (ProbDensity p q s r) = sample s;

-- Define some helper functions
distDefaultValue d = unsafePerformIO' $ sample d;

no_quantile name = error ("Distribution '"++name++"' has no quantile function");

-- Define some basic distributions
builtin gamma_density 3 "gamma_density" "Distribution";
builtin gamma_quantile 3 "gamma_quantile" "Distribution";
builtin builtin_sample_gamma 2 "sample_gamma" "Distribution";
sample_gamma a b = Random (IOAction2 builtin_sample_gamma a b);
gamma (a,b) = ProbDensity (gamma_density a b) (gamma_quantile a b) (sample_gamma a b) (above 0.0);

builtin beta_density 3 "beta_density" "Distribution";
builtin beta_quantile 3 "beta_quantile" "Distribution";
builtin builtin_sample_beta 2 "sample_beta" "Distribution";
sample_beta a b = Random (IOAction2 builtin_sample_beta a b);
beta (a,b) = ProbDensity (beta_density a b) (beta_quantile a b) (sample_beta a b) (between 0.0 1.0);

builtin normal_density 3 "normal_density" "Distribution";
builtin normal_quantile 3 "normal_quantile" "Distribution";
builtin builtin_sample_normal 2 "sample_normal" "Distribution";
sample_normal m s = Random (IOAction2 builtin_sample_normal m s);
normal (m,s) = ProbDensity (normal_density m s) (normal_quantile m s) (sample_normal m s) realLine;

builtin cauchy_density 3 "cauchy_density" "Distribution";
builtin builtin_sample_cauchy 2 "sample_cauchy" "Distribution";
sample_cauchy m s = Random (IOAction2 builtin_sample_cauchy m s);
cauchy (m,s) = ProbDensity (cauchy_density m s) () (sample_cauchy m s) realLine;

builtin laplace_density 3 "laplace_density" "Distribution";
builtin builtin_sample_laplace 2 "sample_laplace" "Distribution";
sample_laplace m s = Random (IOAction2 builtin_sample_laplace m s);
laplace (m,s) = ProbDensity (laplace_density m s) () (sample_laplace m s) realLine;

builtin uniform_density 3 "uniform_density" "Distribution";
builtin builtin_sample_uniform 2 "sample_uniform" "Distribution";
sample_uniform l u = Random (IOAction2 builtin_sample_uniform l u);
uniform (l,u) = ProbDensity (uniform_density l u) () (sample_uniform l u) (between l u);

builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution";
dirichlet_density ps xs = builtin_dirichlet_density (listToVectorDouble ps) (listToVectorDouble xs);
sample_dirichlet ps = do { vs <- mapM (\a->gamma(a,1.0)) ps;
                          return $ map (/(sum vs)) vs};
dirichlet args = ProbDensity (dirichlet_density args) (no_quantile "dirichlet") (sample_dirichlet args) (Simplex (length args) 1.0);

dirichlet' (n,x) = dirichlet (replicate n x);

builtin binomial_density 3 "binomial_density" "Distribution";
builtin builtin_sample_binomial 2 "sample_binomial" "Distribution";
sample_binomial n p = Random (IOAction2 builtin_sample_binomial n p);
binomial (n,p) = ProbDensity (binomial_density n p) (no_quantile "binomial") (sample_binomial n p) (IntegerInterval (Just 0) (Just n));

builtin geometric_density 2 "geometric_density" "Distribution";
builtin builtin_sample_geometric 1 "sample_geometric" "Distribution";
sample_geometric p = Random (IOAction1 builtin_sample_geometric p);
geometric p = ProbDensity (geometric_density p) (no_quantile "geometric") (sample_geometric) (IntegerInterval (Just 0) Nothing);

builtin poisson_density 2 "poisson_density" "Distribution";
builtin builtin_sample_poisson 1 "sample_poisson" "Distribution";
sample_poisson mu = Random (IOAction1 builtin_sample_poisson mu);
poisson mu = ProbDensity (poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (IntegerInterval (Just 0) Nothing);

builtin builtin_sample_bernoulli 1 "sample_bernoulli" "Distribution";
sample_bernoulli p = Random (IOAction1 builtin_sample_bernoulli p);
bernoulli_density p 1 = (doubleToLogDouble p);
bernoulli_density p 0 = (doubleToLogDouble (1.0-p));
bernoulli p = ProbDensity (bernoulli_density p) (no_quantile "bernoulli") (sample_bernoulli p) (IntegerInterval (Just 0) (Just 1));

builtin builtin_sample_exponential 1 "sample_exponential" "Distribution";
builtin exponential_density 2 "exponential_density" "Distribution";
exponential_quantile mu p = gamma_quantile 1.0 mu p;
sample_exponential mu = Random (IOAction1 builtin_sample_exponential mu);
exponential mu = ProbDensity (exponential_density mu) (exponential_quantile mu) (sample_exponential mu) (above 0.0);

builtin crp_density 4 "CRP_density" "Distribution";
crp (alpha,n,d) = ProbDensity (crp_density alpha n d) (no_quantile "crp") (return $ replicate n 0) (ListRange (replicate n (IntegerInterval (Just 0) (Just (n+d-1)))));

mixtureRange ((_,dist1):_) = distRange dist1;
mixture_density ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixture_density l x);
mixture_density [] _ = (doubleToLogDouble 0.0);
sample_mixture ((p1,dist1):l) = dist1;
mixture args = ProbDensity (mixture_density args) (no_quantile "mixture") (sample_mixture args) (mixtureRange args);



categorical p = ProbDensity (q!) (no_quantile "categorical") (return 0) (IntegerInterval (Just 0) (Just (length p - 1)))
                where {q = listArray' $ map doubleToLogDouble p};



logNormal = expTransform' normal;
logExponential = expTransform' exponential;
logGamma = expTransform' gamma;
logLaplace = expTransform' laplace;
logCauchy = expTransform' cauchy;





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

iid (n,d) = list (replicate n d);

plate (n,f) = list $ map f [0..n-1];
  
-- This contains functiosn for working with DiscreteDistribution
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
expTransform (ProbDensity d q s r) = ProbDensity (\x -> (d $ log x)/(doubleToLogDouble x)) (q.log) (do {v <- (ProbDensity d q s r); return $ exp v}) (Range.expTransform r);
expTransform' family args = Distributions.expTransform (family args);
}
