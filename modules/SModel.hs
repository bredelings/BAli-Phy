module SModel where 
{
import Distributions;
import Alphabet;
import Tree;
import Parameters;
    
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
builtin fMutSel_q 4 "fMutSel_q" "SModel";
builtin fMutSel_pi 3 "fMutSel_pi" "SModel";

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

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov' a smap q pi = ReversibleMarkov a smap q pi (get_eigensystem q pi) 1.0 (get_equilibrium_rate a smap q pi);

reversible_markov s (ReversibleFrequency a smap pi r) = reversible_markov' a smap (reversible_rate_matrix s r) pi;

jukes_cantor a = reversible_markov (equ a) (uniform_f_model a);

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
     omega <- uniform 0.0 1.0;
     Log "omega" omega;
     return $ m0 codona s omega
  });

m0_function codona s r = \omega -> reversible_markov (m0 codona s omega) r;

m1a_omega_dist () = do
{
  w1 <- uniform 0.0 1.0;
  Log "w1" w1;

  [f1, f2] <- dirichlet' 2 1.0;
  Log "f1" f1;
  Log "f2" f2;

  return $ DiscreteDistribution [(f1,w1), (f2,1.0)];
};

m2a_omega_dist () = do
{
  dist <- m1a_omega_dist ();

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  return $ extendDiscreteDistribution dist posP posW;
};

m2a_test_omega_dist () = do
{
  dist <- m1a_omega_dist ();

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  posSelection <- bernoulli 0.5;
  Log "posSelection" posSelection;

  let {posW' = if (posSelection == 1) then posW else 1.0};

  return $ extendDiscreteDistribution dist posP posW';
};

m3_omega_dist n_bins = do
{
  omegas <- iid n_bins (uniform 0.0 1.0);
  ps <- dirichlet' n_bins (intToDouble n_bins/2.0);
  Log "omega" omegas;
  Log "f" ps;
  return $ DiscreteDistribution $ zip ps omegas;
};

m3_test_omega_dist n_bins = do
{
  dist <- m3_omega_dist n_bins;

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  posSelection <- bernoulli 0.5;
  Log "posSelection" posSelection;

  let {posW' = if (posSelection == 1) then posW else 1.0};

  return $ extendDiscreteDistribution dist posP posW';
};

-- The M7 is just a beta distribution
-- gamma' = var(x)/(mu*(1-mu)) = 1/(a+b+1) = 1/(n+1)
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

-- The M8a is has f1 of sites in a beta distribution, and f2 are neutral.
m8a_omega_dist n_bins = do
{
  beta_dist <- m7_omega_dist n_bins;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  return $ extendDiscreteDistribution beta_dist posP 1.0;
};

m8a_test_omega_dist n_bins = do
{
  beta_dist <- m7_omega_dist n_bins;

  posW <- logGamma 4.0 0.25;
  Log "posW" posW;

  posP <- beta 1.0 10.0;
  Log "posP" posP;

  posSelection <- bernoulli 0.5;
  Log "posSelection" posSelection;

  let {posW' = if (posSelection == 1) then posW else 0.0};

  return $ extendDiscreteDistribution beta_dist posP posW';
};

dp_omega_dist n_bins = do 
{
  mu <- uniform 0.0 1.0;
  Log "mu" mu;

  omegas <- dirichlet' n_bins 1.0;
  let {p = 1.0/(intToDouble n_bins);
       ave = sum(omegas)*p;
       scale = mu/ave;
       omegas' = map (*scale) omegas};

  Log "omegas" (map (min 1.0) omegas');

  return $ DiscreteDistribution [(p,min omega 1.0)| omega <- omegas'];
};

m1a_model codona s r = Prefix "M1a" $ do
{
  dist <- m1a_omega_dist ();

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m2a_model codona s r = Prefix "M2a" $ do
{
  dist <- m2a_omega_dist ();

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m2a_test_model codona s r = Prefix "M2a_Test" $ do
{
  dist <- m2a_test_omega_dist ();

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m3_model codona n_bins s r = Prefix "M3" $ do
{
  dist <- m3_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

m3_test_model codona n_bins s r = Prefix "M3_Test" $ do
{
  dist <- m3_test_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
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

m8a_test_model codona n_bins s r = Prefix "M8a_Test" $ do
{
  dist <- m8a_test_omega_dist n_bins;

  let {m0w w = reversible_markov (m0 codona s w) r};
  return $ multiParameter m0w dist
};

dp_omega_model codona n_bins s r = Prefix "DP_omega" $ do
{
  dist <- dp_omega_dist n_bins;

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

  fs <- dirichlet' n_bins 1.0;
  sequence_ $ zipWith (\f i -> Log ("f"++show i) f) fs [1..];

  ws' <- iid (n_bins-1) (uniform 0.0 1.0);
  sequence_ $ zipWith (\f i -> Log ("w"++show i) f) ws' [1..];

  let {ws = ws' ++ [1.0]};

  let {d1 = DiscreteDistribution $ zip fs ws;
       d2 = DiscreteDistribution $ zip fs (repeat posW')};

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
  SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     pi <- dirichlet' n_letters 1.0;
     sequence_ $ zipWith (\p l -> Log ("pi"++l) p) pi letters;
     return pi
  }
};

simple_smap a = iotaUnsigned (alphabetSize a);

fMutSel codon_a codon_w omega (ReversibleMarkov _ _ nuc_q nuc_pi _ _ _) =
   let {nuc_a = getNucleotides codon_a;
        smap = simple_smap codon_a;
        codon_w' = listToVectorDouble codon_w;
        q  = fMutSel_q  codon_a codon_w' omega nuc_q;
        pi = fMutSel_pi codon_a codon_w' nuc_pi}
   in reversible_markov' codon_a smap q pi;

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 a w omega nuc_q  = fMutSel a codon_w omega nuc_q
--replicate n_letters (1.0/intToDouble n_letters);
where {codon_w = [w'!aa| codon <- codons,let {aa = translate a codon}];
         w' = listArray' w;
         codons = take n_letters [0..];
         n_letters = alphabetSize a};

fMutSel_model codon_a nuc_rm = Prefix "fMutSel" $ do
{
  omega <- uniform 0.0 1.0;
  Log "omega" omega;

  let {n_letters = alphabetSize codon_a;
       letters = alphabet_letters codon_a};

  ws <- SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     ws <- dirichlet' n_letters 3.0;
     sequence_ $ zipWith (\w l -> Log ("w"++l) w) ws letters;
     return ws
  };

  return $ fMutSel codon_a ws omega nuc_rm;
};

-- Issue: bad mixing on fMutSel model
-- Issue: how to make M2/M8/branchsite/etc versions of fMutSel model?

fMutSel0_model codon_a nuc_rm = Prefix "fMutSel0" $ do
{
  omega <- uniform 0.0 1.0;
  Log "omega" omega;

  let {amino_a = getAminoAcids codon_a;
       n_letters = alphabetSize amino_a;
       letters = alphabet_letters amino_a};

  ws <- SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     ws <- dirichlet' n_letters 3.0;
     sequence_ $ zipWith (\w l -> Log ("w"++l) w) ws letters;
     return ws
  };

  return $ fMutSel0 codon_a ws omega nuc_rm;
};

plus_f a pi = plus_gwf a pi 1.0;

plus_f_model a = Prefix "F" (do {
  pi <- frequencies_model a;
  return (plus_f a pi);
});

plus_gwf a pi f = let {pi' = listToVectorDouble pi} in 
                  ReversibleFrequency a (simple_smap a) pi' (plus_gwF a f pi');

plus_gwf_model a = Prefix "GWF" (do {
  pi <- frequencies_model a;
  f <- uniform 0.0 1.0;
  Log "f" f;
  return (plus_gwf a pi f);
});

uniform_f_model a = let {n_letters = alphabetSize a;
                         pi = replicate n_letters (1.0/intToDouble n_letters);
                         pi' = listToVectorDouble pi} in 
                    ReversibleFrequency a (simple_smap a) pi' (plus_gwF a 1.0 pi');

f1x4 triplet_a nuc_pi = let {nuc_pi' = listToVectorDouble nuc_pi;
                             pi' = f3x4_frequencies triplet_a nuc_pi' nuc_pi' nuc_pi';
                             n_letters = alphabetSize triplet_a} in
                        ReversibleFrequency triplet_a (simple_smap triplet_a) pi' (plus_gwF triplet_a 1.0 pi');

                                        
f1x4_model triplet_a = Prefix "F1x4" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi <- frequencies_model nuc_a;
       return (f1x4 triplet_a nuc_pi);
});

f3x4 triplet_a nuc_pi1 nuc_pi2 nuc_pi3 = let {nuc_pi1' = listToVectorDouble nuc_pi1;
                                              nuc_pi2' = listToVectorDouble nuc_pi2;
                                              nuc_pi3' = listToVectorDouble nuc_pi3;
                                              pi' = f3x4_frequencies triplet_a nuc_pi1' nuc_pi2' nuc_pi3'} in
                                         ReversibleFrequency triplet_a (simple_smap triplet_a) pi' (plus_gwF triplet_a 1.0 pi');

f3x4_model triplet_a = Prefix "F3x4" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi1 <- Prefix "Site1" $ frequencies_model nuc_a;
       nuc_pi2 <- Prefix "Site2" $ frequencies_model nuc_a;
       nuc_pi3 <- Prefix "Site3" $ frequencies_model nuc_a;
       return (f3x4 triplet_a nuc_pi1 nuc_pi2 nuc_pi3);
});

mg94_model triplet_a = Prefix "MG94" 
 (do {
       let {nuc_a = getNucleotides triplet_a};
       nuc_pi <- frequencies_model nuc_a;
       let {nuc_pi' = listToVectorDouble nuc_pi;
            pi' = f3x4_frequencies triplet_a nuc_pi' nuc_pi' nuc_pi';
            nuc_r = plus_gwF nuc_a 1.0 nuc_pi'};
       return $ ReversibleFrequency triplet_a (simple_smap triplet_a) pi' (muse_gaut_matrix triplet_a nuc_r nuc_r nuc_r)
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
            pi' = f3x4_frequencies triplet_a nuc_pi1' nuc_pi2' nuc_pi3'};
       return $ ReversibleFrequency triplet_a (simple_smap triplet_a) pi' (muse_gaut_matrix triplet_a nuc_r1 nuc_r2 nuc_r3)
});

gamma_model base n = Prefix "Gamma"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;
     let {a = 1.0/b; 
          b = sigmaOverMu^2;
          discretize n = uniformDiscretize (quantile (gamma a b)) n};
     Log "a" a;
     Log "b" b;
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
     Log "a" a;
     Log "b" b;

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


branch_transition_p t smodel branch_cat_list ds b = vector_Matrix_From_List $ branchTransitionP (getNthMixture smodel (branch_cat_list!!b)) (ds!b);

transition_p_index t smodel branch_cat_list ds = mkArray (numBranches t) (branch_transition_p t smodel branch_cat_list ds);

distance_model scale branch =
(do {
   m <- new_modifiable;
   add_parameter ("*Scale"++show scale++"."++show (branch+1)) m;
   return m
   });

a_branch_mean_model n = 
(do {
   mu <- gamma 0.5 2.0;
   Log ("mu"++show n) mu;
   return mu
});

branch_mean_model n = Prefix "Main" (mapM (\i -> a_branch_mean_model i) [1..n]);

a_branch_length_model dist i =
(do {
  t <- dist;
  Log ("*T"++show i) t;
  return t
});

iid_branch_length_model t dist = SamplingRate 0.0 $ mapM (\i -> a_branch_length_model dist i) [1..numBranches t];

iid_branch_length_model_exp t = iid_branch_length_model t (exponential (1.0/(intToDouble (numBranches t))));

iid_branch_length_model_gamma t = iid_branch_length_model t (gamma 0.5 (2.0/(intToDouble (numBranches t))));

reversible_markov_model s r = return $ reversible_markov s r;

unit_model m = return $ MixtureModel (DiscreteDistribution [(1.0,m)]);

mmm m = return $ MixtureModels [m];

}
