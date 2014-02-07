module SModel where 
{
import Distributions;
import Alphabet;

builtin f3x4_frequencies 4 "f3x4_frequencies";
builtin muse_gaut_matrix 4 "muse_gaut_matrix" "SModel";
builtin plus_gwF 3 "plus_gwF";
builtin gtr 7 "gtr";
builtin m0 3 "m0";
builtin lExp 3 "lExp" "SModel";
builtin reversible_rate_matrix 2 "reversible_rate_matrix" "SModel";
builtin get_eigensystem 2 "get_eigensystem" "SModel";
builtin get_equilibrium_rate 4 "get_equilibrium_rate" "SModel";
builtin singlet_to_triplet_exchange 2 "singlet_to_triplet_exchange" "SModel";
builtin equ 1 "equ" "SModel";
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
-- ag at ac gt gc tc
     s <- dirichlet [2.0/8.0, 1.0/8.0, 1.0/8.0, 1.0/8.0, 1.0/8.0, 2.0/8.0];
     Log "s" s;
     return $ gtr nuca (s!!0) (s!!1) (s!!2) (s!!3) (s!!4) (s!!5)
});

m0_model codona s = Prefix "M0"
  (do {
     omega <- logLaplace 0.0 0.25;
     Log "omega" omega;
     return $ m0 codona s omega
  });

m0_function codona s r = \omega -> reversible_markov (m0 codona s omega) r;

m2a_model codona s r = Prefix "M2a" 
(do {
  f <- dirichlet [10.0, 10.0, 1.0];
  let {fConserved = f!!0;
       fNeutral = f!!1;
       fSelection = f!!2};
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

x3_model s a = do {
 s' <- s (getNucleotides a);
 return $ singlet_to_triplet_exchange a s'
};

hkyx3_model a = x3_model hky_model;

tnx3_model a = x3_model tn_model;

gtrx3_model a = x3_model gtr_model;

frequencies_model a = do {
  let {n_letters = alphabetSize a};
  pi <- dirichlet' n_letters 1.0;
  Log "pi" pi;
  return pi
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

gamma_model base n = Prefix "Gamma"
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;
     let {a = 1.0/b; 
          b = sigmaOverMu^2;
          discretize n = uniformDiscretize (quantile (gamma a b)) n};
     return $ multiRate base (discretize n)
});

reversible_markov_model s r = return $ reversible_markov s r;

unit_model m = return $ MixtureModel (DiscreteDistribution [(1.0,m)]);

mmm m = return $ MixtureModels [m];

}
