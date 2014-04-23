module SModel where 
{
import Distributions;
import Alphabet;

builtin f3x4_frequencies 4 "f3x4_frequencies" "SModel";
builtin muse_gaut_matrix 4 "muse_gaut_matrix" "SModel";
builtin plus_gwF 3 "plus_gwF" "SModel";
builtin gtr 7 "gtr" "SModel";
builtin m0 3 "m0" "SModel";
builtin lExp 3 "lExp" "SModel";
builtin reversible_rate_matrix 2 "reversible_rate_matrix" "SModel";
builtin get_eigensystem 2 "get_eigensystem" "SModel";
builtin get_equilibrium_rate 4 "get_equilibrium_rate" "SModel";
builtin singlet_to_triplet_exchange 2 "singlet_to_triplet_exchange" "SModel";
builtin empirical 2 "empirical" "SModel";
builtin pam 1 "pam" "SModel";
builtin jtt 1 "jtt" "SModel";
builtin wag 1 "wag" "SModel";
builtin lg 1 "lg" "SModel";



data ReversibleMarkov = ReversibleMarkov a b c d e f g;
data ReversibleFrequency = ReversibleFrequency a b c d;
data F81 = F81 a b c d;
data MixtureModel = MixtureModel a;
data MixtureModels = MixtureModels a;

equ a = gtr a 1.0 1.0 1.0 1.0 1.0 1.0;
hky a k = gtr a k 1.0 1.0 1.0 1.0 k;
tn a k1 k2 = gtr a k1 1.0 1.0 1.0 1.0 k2;

scale x (ReversibleMarkov a s q pi l t r) = ReversibleMarkov a s q pi l (x*t) (x*r);

multiParameter f (DiscreteDistribution d) = MixtureModel (DiscreteDistribution (fmap2 f d));

multiRate m d = multiParameter (\x->(scale x m)) d;

rate (ReversibleMarkov a s q pi l t r) = r;
rate (MixtureModel d) = average (fmap2 rate d);
     
qExp (ReversibleMarkov a s q pi l t r) = lExp l pi t;

branchTransitionP (MixtureModel (DiscreteDistribution l)) t = let {r = rate (MixtureModel (DiscreteDistribution l))} 
                                                              in map (\x -> qExp (scale (t/r) (snd x))) l;

reversible_markov s (ReversibleFrequency a smap pi r) = let {q = reversible_rate_matrix s r} 
                                                        in ReversibleMarkov a smap q pi (get_eigensystem q pi) 1.0 (get_equilibrium_rate a smap q pi);

nBaseModels (MixtureModel (DiscreteDistribution l)) = length l;
nBaseModels (MixtureModels (m:ms)) = nBaseModels m;

baseModel (MixtureModel (DiscreteDistribution l)) i = snd (l !! i);

stateLetters (ReversibleMarkov _ smap _ _ _ _ _) = smap;
stateLetters (F81 _ smap _ _ ) = smap;
stateLetters (MixtureModel l) = stateLetters (baseModel (MixtureModel l) 0);
stateLetters (MixtureModels (m:ms)) = stateLetters m;

nStates m = sizeOfVectorUnsigned (stateLetters m);
  
getAlphabet (ReversibleMarkov a _ _ _ _ _ _) = a;
getAlphabet (F81 a _ _ _) = a;
getAlphabet (MixtureModel l) = getAlphabet (baseModel (MixtureModel l) 0);
getAlphabet (MixtureModels (m:ms)) = getAlphabet m;

frequencies (ReversibleMarkov _ _ _ pi _ _ _) = pi;
frequencies (F81 _ _ _ pi) = pi;

componentFrequencies (MixtureModel d)       i = frequencies (baseModel (MixtureModel d) i);
componentFrequencies (MixtureModels (m:ms)) i = componentFrequencies m i;

distribution (MixtureModel (DiscreteDistribution l)) = map fst l;
distribution (MixtureModels (m:ms))                  = distribution m;

getNthMixture (MixtureModels l) i = l !! i;

unwrapMM (MixtureModel dd) = dd;

mixMixtureModels l dd = MixtureModel (mixDiscreteDistributions l (map unwrapMM dd));

--
equ_model nuca = return $ equ nuca;

hky_model nuca = Prefix "HKY" 
  (do {
     kappa <- logLaplace (log 2.0) 0.25 ;
     Log "kappa" kappa;
     return $ hky nuca kappa
});

tn_model nuca = Prefix "TN" 
  (do {
     kappaPur <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPur" kappaPur;
     kappaPyr <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPyr" kappaPyr;
     return $ tn nuca kappaPur kappaPyr
});

gtr_model nuca = Prefix "GTR" 
  (do {
     [ag,at,ac,gt,gc,tc] <- dirichlet [2.0/8.0, 1.0/8.0, 1.0/8.0, 1.0/8.0, 1.0/8.0, 2.0/8.0];
     Log "ag" ag;
     Log "at" at;
     Log "ac" ac;
     Log "gt" gt;
     Log "gc" gc;
     Log "tc" tc;
     return $ gtr nuca ag at ac gt gc tc
});

m0_model codona s = Prefix "M0"
  (do {
     omega <- logLaplace 0.0 0.25;
     Log "omega" omega;
     return $ m0 codona s omega
  });

m0_function codona s r = \omega -> reversible_markov (m0 codona s omega) r;

m1a_model codona s r = Prefix "M1a" 
(do {
  [fConserved, fNeutral] <- dirichlet [10.0, 11.0];
  Log "fConserved" fConserved;
  Log "fNeutral" fNeutral;

  omega1 <- uniform 0.0 1.0;
  Log "omega1" omega1;

  let {dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0)]};
  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
});

m2a_model codona s r = Prefix "M2a" 
(do {
  [fConserved, fNeutral, fSelection] <- dirichlet [10.0, 10.0, 1.0];
  Log "fConserved" fConserved;
  Log "fNeutral" fNeutral;
  Log "fSelection" fSelection;

  omega1 <- uniform 0.0 1.0;
  Log "omega1" omega1;

  omega3 <- logGamma 4.0 0.25;
  Log "omega3" omega3;

  let {dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0),(fSelection,omega3)]};
  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
});

m2a_test_model codona s r = Prefix "M2a_Test" 
(do {
  [fConserved, fNeutral, fSelection] <- dirichlet [10.0, 10.0, 1.0];
  Log "fConserved" fConserved;
  Log "fNeutral" fNeutral;
  Log "fSelection" fSelection;

  omega1 <- uniform 0.0 1.0;
  Log "omega1" omega1;

  omega3 <- logGamma 4.0 0.25;
  Log "omega3" omega3;

  pos_selection <- bernoulli 0.5;
  Log "pos_selection" pos_selection;

  let {omega3' = if pos_selection then omega3 else 1.0};

  let {dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0),(fSelection,omega3')]};
  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
});


-- The M7 is just a beta distribution
m7_omega_dist n_bins = do 
{
  mu <- uniform 0.0 1.0;
  Log "mu" mu;

  gamma <- beta 1.0 10.0;
  -- sigma^2/mu
  Log "gamma" gamma;

  let {cap = min (mu/(1.0+mu)) ((1.0-mu)/(2.0-mu));
       gamma' = gamma*cap;
       n = (1.0/gamma')-1.0;
       a = n*mu;
       b = n*(1.0 - mu)};

  Log "a" a;
  Log "b" b;

  return $ uniformDiscretize (quantile (beta a b)) n_bins;
};

-- The M8 is a beta distribution, where a fraction posP of sites have omega posW
m8_omega_dist n_bins = do
{
  beta_dist <- m7_omega_dist n_bins;

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  return $ extendDiscreteDistribution beta_dist posP posW;
};

-- The M8a is has f0 of sites in a beta distribution, and f1 are neutral.
m8a_omega_dist n_bins = do
{
  beta_dist <- m7_omega_dist n_bins;

  [f0,f1] <- dirichlet' 2 1.0;
  Log "f0" f0;
  Log "f1" f1;

  return $ extendDiscreteDistribution beta_dist f1 1.0;
};

-- The M8a is has f0 of sites in a beta distribution, and f1 are neutral.
m8b_omega_dist n_bins = do
{
  dist <- m8a_omega_dist n_bins;

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  return $ extendDiscreteDistribution dist posP posW;
};

m8b_test_omega_dist n_bins = do
{
  dist <- m8a_omega_dist n_bins;

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  posSelection <- bernoulli 0.5;
  Log "posSelection" posSelection;

  let {w' = if (posSelection == 1) then posW else 1.0};

  return $ extendDiscreteDistribution dist posP w';
};



m7_model codona n_bins s r = Prefix "M7" $ do
{
  dist <- m7_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m8_model codona n_bins s r = Prefix "M8" $ do
{
  dist <- m8_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m8a_model codona n_bins s r = Prefix "M8a" $ do
{
  dist <- m8a_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m8b_model codona n_bins s r = Prefix "M8b" $ do
{
  dist <- m8b_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m8b_test_model codona n_bins s r = Prefix "M8b_Test" $ do
{
  dist <- m8b_test_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

branch_site_test_model codona n_bins s r = Prefix "BranchSiteTest"
(do {
  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posSelection <- bernoulli 0.5;
  Log "posSelection" posSelection;

  let {posW' = if (posSelection == 1) then posW else 1.0};

  [f0,f1] <- dirichlet' 2 1.0;
  Log "f0" f0;
  Log "f1" f1;

  w0 <- uniform 0.0 1.0;
  Log "w0" w0;

  let {d1 = DiscreteDistribution [(f0,w0),(f1,1.0)];
       d2 = DiscreteDistribution [(f0,posW'),(f1,posW')]};

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  let {m0w w = reversible_markov (m0 codona s w) r;
       mixture1 = multiParameter m0w (mixDiscreteDistributions [1.0-posP, posP] [d1,d1]);
       mixture2 = multiParameter m0w (mixDiscreteDistributions [1.0-posP, posP] [d1,d2])};

  return $ MixtureModels [mixture1,mixture2]
});

x3_model s a = do {
 s' <- s (getNucleotides a);
 return $ singlet_to_triplet_exchange a s'
};

hkyx3_model a = x3_model hky_model;

tnx3_model a = x3_model tn_model;

gtrx3_model a = x3_model gtr_model;

frequencies_model a = do {
  let {n_letters = alphabetSize a;
       letters = alphabet_letters a};
  SamplingRate (1.0/intToDouble n_letters) $ do {
     pi <- dirichlet' n_letters 1.0;
     sequence_ $ zipWith (\p l -> Log ("pi"++l) p) pi letters;
     return pi
  }
};

plus_f_model a = Prefix "F" (do {
  pi <- frequencies_model a;
  let {n_letters = alphabetSize a};
  let {pi' = listToVectorDouble pi};
  return (ReversibleFrequency a (iotaUnsigned n_letters) pi' (plus_gwF a 1.0 pi'))
});

plus_gwf_model a = Prefix "GWF" (do {
  pi <- frequencies_model a;
  f <- uniform 0.0 1.0;
  Log "f" f;
  let {n_letters = alphabetSize a};
  let {pi' = listToVectorDouble pi};
  return (ReversibleFrequency a (iotaUnsigned n_letters) pi' (plus_gwF a 1.0 pi'))
});

uniform_f_model a = let {n_letters = alphabetSize a;
                         pi = replicate n_letters (1.0/intToDouble n_letters);
                         pi' = listToVectorDouble pi} in 
                    ReversibleFrequency a (iotaUnsigned n_letters) pi' (plus_gwF a 1.0 pi');

f1x4_model triplet_a = Prefix "F1x4" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi <- frequencies_model nuc_a;
       let {nuc_pi' = listToVectorDouble nuc_pi;
            pi' = f3x4_frequencies triplet_a nuc_pi' nuc_pi' nuc_pi';
            n_letters = alphabetSize triplet_a};
       return $ ReversibleFrequency triplet_a (iotaUnsigned n_letters) pi' (plus_gwF triplet_a 1.0 pi')
});

f3x4_model triplet_a = Prefix "F1x4" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi1 <- Prefix "Site1" $ frequencies_model nuc_a;
       nuc_pi2 <- Prefix "Site2" $ frequencies_model nuc_a;
       nuc_pi3 <- Prefix "Site3" $ frequencies_model nuc_a;
       let {nuc_pi1' = listToVectorDouble nuc_pi1;
            nuc_pi2' = listToVectorDouble nuc_pi2;
            nuc_pi3' = listToVectorDouble nuc_pi3;
            pi' = f3x4_frequencies triplet_a nuc_pi1' nuc_pi2' nuc_pi3';
            n_letters = alphabetSize triplet_a};
       return $ ReversibleFrequency triplet_a (iotaUnsigned n_letters) pi' (plus_gwF triplet_a 1.0 pi')
});

mg94_model triplet_a = Prefix "MG94" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi <- frequencies_model nuc_a;
       let {nuc_pi' = listToVectorDouble nuc_pi;
            pi' = f3x4_frequencies triplet_a nuc_pi' nuc_pi' nuc_pi';
            nuc_r = plus_gwF nuc_a 1.0 nuc_pi';
            n_letters = alphabetSize triplet_a};
       return $ ReversibleFrequency triplet_a (iotaUnsigned n_letters) pi' (muse_gaut_matrix triplet_a nuc_r nuc_r nuc_r)
});

mg94w9_model triplet_a = Prefix "MG94w9" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi1 <- Prefix "Site1" $ frequencies_model nuc_a;
       nuc_pi2 <- Prefix "Site2" $ frequencies_model nuc_a;
       nuc_pi3 <- Prefix "Site3" $ frequencies_model nuc_a;
       let {nuc_pi1' = listToVectorDouble nuc_pi1;
            nuc_pi2' = listToVectorDouble nuc_pi2;
            nuc_pi3' = listToVectorDouble nuc_pi3;
            nuc_r1   = plus_gwF nuc_a 1.0 nuc_pi1';
            nuc_r2   = plus_gwF nuc_a 1.0 nuc_pi2';
            nuc_r3   = plus_gwF nuc_a 1.0 nuc_pi3';
            pi' = f3x4_frequencies triplet_a nuc_pi1' nuc_pi2' nuc_pi3';
            n_letters = alphabetSize triplet_a};
       return $ ReversibleFrequency triplet_a (iotaUnsigned n_letters) pi' (muse_gaut_matrix triplet_a nuc_r1 nuc_r2 nuc_r3)
});

gamma_model base n = Prefix "Gamma"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;
     let {a = 1.0/b; 
          b = sigmaOverMu^2;
          discretize n = uniformDiscretize (quantile (gamma a b)) n};
     return $ multiRate base (discretize n)
});

plus_inv_model dist = do 
{
  pInv <- beta 1.0 2.0;
  Log "pInv" pInv;
  return $ extendDiscreteDistribution dist pInv 0.0
};

inv_model base = Prefix "INV" $ do
{
     let {dist = DiscreteDistribution [(1.0,1.0)]};

     dist2 <- plus_inv_model dist;

     return $ multiRate base dist2
};

gamma_inv_model base n = Prefix "GammaINV"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;

     let {a = 1.0/b; 
          b = sigmaOverMu^2;
          dist = uniformDiscretize (quantile (gamma a b)) n};

     dist2 <- plus_inv_model dist;

     return $ multiRate base dist2
});

log_normal_model base n = Prefix "LogNormal"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;

     let {x = log(1.0+sigmaOverMu^2);
          lmu = -0.5*x;
          lsigma = sqrt x;
          dist = uniformDiscretize (quantile (logNormal lmu lsigma)) n};
     return $ multiRate base dist
});

log_normal_inv_model base n = Prefix "LogNormalInv"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;

     let {x = log(1.0+sigmaOverMu^2);
          lmu = -0.5*x;
          lsigma = sqrt x;
          dist = uniformDiscretize (quantile (logNormal lmu lsigma)) n};

     dist2 <- plus_inv_model dist;

     return $ multiRate base dist2
});

dp_model base n = Prefix "DP"
(do {
   fraction <- dirichlet' n (1.0 + (intToDouble n/2.0));
   rates    <- dirichlet' n 2.0;
   let {dist = zip fraction rates;
        dist' = quicksortWith (\(f,r)->r) dist;
        x = unzip dist';
        fs = fst x;
        rs = snd x};

   sequence_ $ zipWith (\f i -> Log ("f"++show i) f) fraction [1..];
   sequence_ $ zipWith (\f i -> Log ("rates"++show i) f) rates [1..];
--   mapM_ (\i -> Log ("f"++show i) (fs!!i)) [0..];
--   mapM_ (\i -> Log ("rate"++show i) (rs!!i)) [0..];

   return $ multiRate base (DiscreteDistribution dist)
});

a_branch_mean_model n = 
(do {
   mu <- gamma 0.5 2.0;
   Log ("mu"++show n) mu;
   return ()
});

branch_mean_model n = Prefix "Main" (mapM_ (\i -> a_branch_mean_model i) [1..n]);

reversible_markov_model s r = return $ reversible_markov s r;

unit_model m = return $ MixtureModel (DiscreteDistribution [(1.0,m)]);

mmm m = return $ MixtureModels [m];

}
