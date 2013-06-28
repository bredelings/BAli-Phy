module Distributions where
{
import Range;

builtin exponential_density 2 "exponential_density" "Distribution";

builtin builtin_gamma_density 3 "gamma_density" "Distribution";
builtin builtin_gamma_quantile 3 "gamma_quantile" "Distribution";

builtin builtin_beta_density 3 "beta_density" "Distribution";
builtin builtin_beta_quantile 3 "beta_quantile" "Distribution";

builtin builtin_normal_density 3 "normal_density" "Distribution";
builtin builtin_normal_quantile 3 "normal_quantile" "Distribution";

builtin builtin_cauchy_density 3 "cauchy_density" "Distribution";
builtin builtin_laplace_density 3 "laplace_density" "Distribution";
builtin builtin_dirichlet_density 2 "dirichlet_density" "Distribution";
builtin builtin_uniform_density 3 "uniform_density" "Distribution";

builtin builtin_binomial_density 3 "binomial_density" "Distribution";
builtin geometric_density 2 "geometric_density" "Distribution";

builtin crp_density 4 "CRP_density" "Distribution";

data ProbDensity = ProbDensity a b c d;
data DiscreteDistribution a = DiscreteDistribution [(Double,a)];

pairs f (x:y:t) = f x y : pairs f t;
pairs _ t       = t;

foldt f z []  = z;
foldt f _ [x] = x;
foldt f z xs  = foldt f z (pairs f xs);

balanced_product xs = foldt (*) (doubleToLogDouble 1.0) xs;

density (ProbDensity d _ _ _) = d;
quantile (ProbDensity _ q _ _) = q;
distDefaultValue (ProbDensity _ _ v _) = v;
distRange (ProbDensity _ _ _ r) = r;

gammaDensity (a,b) x = builtin_gamma_density a b x;
gammaQuantile (a,b) p = builtin_gamma_quantile a b p;
betaDensity (a,b) x = builtin_beta_density a b x;
betaQuantile (a,b) p = builtin_beta_quantile a b p;
normalDensity (mu,sigma) x =  builtin_normal_density mu sigma x;
normalQuantile (mu,sigma) p =  builtin_normal_quantile mu sigma p;

cauchyDensity (m,s) x = builtin_cauchy_density m s x;

laplaceDensity (m,s) x = builtin_laplace_density m s x;

dirichletDensity ps xs = builtin_dirichlet_density (listToVectorDouble ps) (listToVectorDouble xs);

uniformDensity (min,max) x = builtin_uniform_density min max x;

exponentialQuantile mu p = gammaQuantile (1.0,mu) p;

mixtureDensity ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixtureDensity l x);
mixtureDensity [] _ = (doubleToLogDouble 0.0);

mixtureDefault ((p1,dist1):l) = distDefaultValue dist1;
dirichletDefault l = let {n = length l} in replicate n $ 1.0/(intToDouble n);
iidDefault l = let {n = length l} in replicate n 1.0/(intToDouble n);

mixtureRange ((_,dist1):_) = distRange dist1;

bernoulliDensity p b = if b then (doubleToLogDouble p) else (doubleToLogDouble (1.0-p));

bernoulli args = ProbDensity (bernoulliDensity args) (error "Bernoulli has no quantile") False TrueFalseRange;

categorical p = ProbDensity (q!) (error "Categorical has no quantiles") 0 (IntegerInterval (Just 0) (Just (length p - 1)))
                where {q = listArray' $ map doubleToLogDouble p};

beta args = ProbDensity (betaDensity args) (betaQuantile args) ((\(a,b)->a/(a+b)) args) (between 0.0 1.0);
uniform (l,u) = ProbDensity (uniformDensity (l,u)) () ((l+u)/2.0) (between l u);

normal args = ProbDensity (normalDensity args) (normalQuantile args) 0.0 realLine;
exponential mu = ProbDensity (exponential_density mu) (exponentialQuantile mu) mu (above 0.0);
gamma args = ProbDensity (gammaDensity args) (gammaQuantile args) ((\(a,b)->a*b) args) (above 0.0);
laplace args = ProbDensity (laplaceDensity args) () ((\(m,s)->m) args) realLine;
cauchy args = ProbDensity (cauchyDensity args) () 0.0 realLine;

logNormal = expTransform' normal;
logExponential = expTransform' exponential;
logGamma = expTransform' gamma;
logLaplace = expTransform' laplace;
logCauchy = expTransform' cauchy;

geometric_quantile p = error "geometric currently has no quantile";

geometric_initial p = 1;

geometric p = ProbDensity (geometric_density p) (geometric_quantile p) (geometric_initial p) (IntegerInterval (Just 0) Nothing);

dirichlet args = ProbDensity (dirichletDensity args) (error "Dirichlet has no quantiles") (dirichletDefault args) (Simplex (length args) 1.0);
dirichlet' (n,x) = dirichlet (replicate n x);

mixture args = ProbDensity (mixtureDensity args) (error "Mixture has no quantiles") (mixtureDefault args) (mixtureRange args);

binomial_density (n,p) k = builtin_binomial_density n p k;

binomial (n,p) = ProbDensity (binomial_density (n,p)) (error "binomial has no quantile") (doubleToInt $ (intToDouble n)*p) (IntegerInterval (Just 0) (Just n));

listDensity ds xs = if (length ds == length xs) then pr else (doubleToLogDouble 0.0)
  where {densities = zipWith density ds xs;
         pr = balanced_product densities};

list dists = ProbDensity (listDensity dists) quantiles (map distDefaultValue dists) (ListRange (map distRange dists))
  where { quantiles = (error "list distribution has no quantiles") };

crp (alpha,n,d) = ProbDensity (crp_density alpha n d) quantiles (replicate n 0) (ListRange (replicate n (IntegerInterval (Just 0) (Just (n+d-1)))))
  where { quantiles = (error "crp distribution has no quantiles") };

iid (n,d) = list (replicate n d);

plate (n,f) = list $ map f [0..n-1];
  
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

uniformDiscretize q n = DiscreteDistribution [( 1.0/n', (2.0*i'+1.0)/n' ) | i <- take n [0..], let {n' = intToDouble n;i'=intToDouble i}];

expTransform (ProbDensity d q v r) = ProbDensity (\x -> (d $ log x)/(doubleToLogDouble x)) (q.log) (exp v) (Range.expTransform r);
expTransform' family args = Distributions.expTransform (family args);
}
