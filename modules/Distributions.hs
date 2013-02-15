module Distributions where
{
import Range;

data ProbDensity = ProbDensity a b c d;
data DiscreteDistribution a = DiscreteDistribution [(Double,a)];

density (ProbDensity d _ _ _) = d;
quantile (ProbDensity _ q _ _) = q;
distDefaultValue (ProbDensity _ _ v _) = v;
distRange (ProbDensity _ _ _ r) = r;

gammaDensity (a,b) x = builtinGammaDensity a b x;
gammaQuantile (a,b) p = builtinGammaQuantile a b p;
betaDensity (a,b) x = builtinBetaDensity a b x;
betaQuantile (a,b) p = builtinBetaQuantile a b p;
normalDensity (mu,sigma) x =  builtinNormalDensity mu sigma x;
normalQuantile (mu,sigma) p =  builtinNormalQuantile mu sigma p;

cauchyDensity (m,s) x = builtinCauchyDensity m s x;

laplaceDensity (m,s) x = builtinLaplaceDensity m s x;

dirichletDensity ps xs = builtinDirichletDensity (listToVectorDouble ps) (listToVectorDouble xs);

uniformDensity (min,max) x = builtinUniformDensity min max x;

exponentialQuantile mu p = gammaQuantile (1.0,mu) p;

mixtureDensity ((p1,dist1):l) x = (doubleToLogDouble p1)*(density dist1 x) + (mixtureDensity l x);
mixtureDensity [] _ = (doubleToLogDouble 0.0);

mixtureDefault ((p1,dist1):l) = distDefaultValue dist1;
dirichletDefault l = let {n = length l} in replicate n $ 1.0/(intToDouble n);
iidDefault l = let {n = length l} in replicate n 1.0/(intToDouble n);

mixtureRange ((_,dist1):_) = distRange dist1;

bernoulliDensity p b = if b then (doubleToLogDouble p) else (doubleToLogDouble (1.0-p));

bernoulli args = (ProbDensity (bernoulliDensity args) (error "Bernoulli has no quantile") True TrueFalseRange);
beta args = (ProbDensity (betaDensity args) (betaQuantile args) ((\(a,b)->a/(a+b)) args) (between 0.0 1.0));
uniform (l,u) = (ProbDensity (uniformDensity (l,u)) () ((l+u)/2.0) (between l u));

normal args = (ProbDensity (normalDensity args) (normalQuantile args) 0.0 realLine);
exponential mu = (ProbDensity (exponentialDensity mu) (exponentialQuantile mu) mu (above 0.0));
gamma args = (ProbDensity (gammaDensity args) (gammaQuantile args) ((\(a,b)->a*b) args) (above 0.0));
laplace args = (ProbDensity (laplaceDensity args) () ((\(m,s)->m) args) realLine);
cauchy args = (ProbDensity (cauchyDensity args) () 0.0 realLine);

logNormal = expTransform' normal;
logExponential = expTransform' exponential;
logGamma = expTransform' gamma;
logLaplace = expTransform' laplace;
logCauchy = expTransform' cauchy;

dirichlet args = (ProbDensity (dirichletDensity args) (error "Dirichlet has no quantiles") (dirichletDefault args) (Simplex (length args) 1.0));
dirichlet' (n,x) = dirichlet (replicate n x);

mixture args = ProbDensity (mixtureDensity args) (error "Mixture has no quantiles") (mixtureDefault args) (mixtureRange args);

listDensity ds xs = if (length ds == length xs) then pr else (doubleToLogDouble 0.0)
  where {densities = zipWith density ds xs;
         pr = foldl' (*) (doubleToLogDouble 1.0) densities};

list dists = ProbDensity (listDensity dists) quantiles (map distDefaultValue dists) (ListRange (map distRange dists))
  where { quantiles = (error "list distribution has no quantiles") };

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
uniformDiscretize q n = let {n' = (intToDouble n)} in DiscreteDistribution (map (\i->(1.0/n',q (2.0*i+1.0)/n')) (take n [0..]));

expTransform (ProbDensity d q v r) = (ProbDensity (\x -> (d $ log x)/(doubleToLogDouble x)) (q.log) (exp v) (Range.expTransform r));
expTransform' family args = Distributions.expTransform (family args);
}
