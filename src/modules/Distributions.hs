module Distributions where
{
import Range;

gammaDensity (a,b) x = builtinGammaDensity a b x;
gammaQuantile (a,b) p = builtinGammaQuantile a b p;
logGammaDensity (a,b) x = builtinLogGammaDensity (a,b) x;
betaDensity (a,b) x = builtinBetaDensity a b x;
betaQuantile (a,b) p = builtinBetaQuantile a b p;
normalDensity (mu,sigma) x =  builtinNormalDensity mu sigma x;

logNormalDensity (mu,sigma) x = builtinLogNormalDensity mu sigma x;
logNormalQuantile (mu,sigma) x = builtinLogNormalQuantile mu sigma x;

cauchyDensity (m,s) x = builtinCauchyDensity m s x;

laplaceDensity (m,s) x = builtinLaplaceDensity m s x;

dirichletDensity ps xs = builtinDirichletDensity (listToVectorDouble ps) (listToVectorDouble xs);

logLaplaceDensity (m,s) x = builtinLogLaplaceDensity m s x;

uniformDensity (min,max) x = builtinUniformDensity min max x;

exponentialQuantile mu p = gammaQuantile (1.0,mu) p;

mixtureDensity ((p1,(ProbDensity _ density1 _ _ _,args1)):l) x = (doubleToLogDouble p1)*(density1 args1 x)+(mixtureDensity l x);
mixtureDensity [] _ = (doubleToLogDouble 0.0);

mixtureDefault ((p1,(ProbDensity _ _ _ d _,args1)):l) = (d args1);
dirichletDefault l = let {n = length l} in (take n (repeat 1.0/(intToDouble n)));
iidDefault l = let {n = length l} in (take n (repeat 1.0/(intToDouble n)));

betaDist  =       (ProbDensity "Beta"        betaDensity        betaQuantile (\args->0.5) 0);

bernoulliDensity p b = if b then (doubleToLogDouble p) else (doubleToLogDouble (1.0-p));
bernoulli args = (ProbDensity "Bernoulli" bernoulliDensity (error "Bernoulli has no quantile") (\_->True) (), args);
normal args = (ProbDensity "Normal" normalDensity () (\_->0.0) (\_->realLine), args);
exponential args = (ProbDensity "Exponential" exponentialDensity exponentialQuantile (\mu->mu) (\_->above 0.0), args);
gamma args = (ProbDensity "Gamma" gammaDensity gammaQuantile (\(a,b)->a*b) (\_->above 0.0), args);
betaD args = (ProbDensity "Beta"        betaDensity        betaQuantile (\(a,b)->a/(a+b)) (\_->between 0.0 1.0), args);
mixture args = (ProbDensity "Mixture" mixtureDensity () mixtureDefault (\_->()), args);
dirichlet args = (ProbDensity "Dirichlet" dirichletDensity (error "Dirichlet has no quantiles") () (), args);
laplace args = (ProbDensity "Laplace" laplaceDensity () (\(m,s)->m) (\_->realLine), args);
logLaplace args = (ProbDensity "LogLaplace" logLaplaceDensity () (\(m,s)->log m) (\_->above 0.0), args);
logExponential args = (ProbDensity "LogExponential" logExponentialDensity () (\mu->log mu) (\_->above 0.0), args);
logNormal args = (ProbDensity "LogNormal" logNormalDensity logNormalQuantile () (\_->above 0.0), args);
logGamma args = (ProbDensity "LogGamma" logGammaDensity () () (\_->above 0.0), args);
uniform args = (ProbDensity "Uniform" uniformDensity () () (\(l,u)->between l u), args);
cauchy args = (ProbDensity "Cauchy" cauchyDensity () () realLine, args);
distRange (ProbDensity _ _ _ _ r,args) = r args;

iidDensity (n,((ProbDensity _ density _ _ _),args)) xs = let {densities = (map (density args) xs) ; 
                                                              pr = foldl' (*) (doubleToLogDouble 1.0) densities} 
                                                         in if (length xs == n) then pr else (doubleToLogDouble 0.0);
iid args = (ProbDensity "i.i.d." iidDensity () () (\_->()), args );

plateDensity (n,f) xs = let {xs' = zip [1..] xs;
                             densities = map (\(i,x) -> case (f i) of {(ProbDensity _ d _ _ _, a) -> d a x}) xs';
                             pr = foldl' (*) (doubleToLogDouble 1.0) densities}
                        in if (length xs == n) then pr else (doubleToLogDouble 0.0);
plate args = (ProbDensity "Plate" plateDensity () () (\_->()), args );
}
