module SModel where 
{
import Distributions;
import Alphabet;
import Tree;
import Parameters;

builtin f3x4_frequencies_builtin 4 "f3x4_frequencies" "SModel";
builtin muse_gaut_matrix 4 "muse_gaut_matrix" "SModel";
builtin builtin_plus_gwf 3 "plus_gwF" "SModel";
builtin builtin_average_frequency 1 "average_frequency" "SModel";
builtin builtin_gtr 2 "gtr" "SModel";
builtin m0 3 "m0" "SModel";
builtin lExp 3 "lExp" "SModel";
builtin reversible_rate_matrix 2 "reversible_rate_matrix" "SModel";
builtin get_eigensystem 2 "get_eigensystem" "SModel";
builtin get_equilibrium_rate 4 "get_equilibrium_rate" "SModel";
builtin singlet_to_triplet_exchange 2 "singlet_to_triplet_exchange" "SModel";
builtin builtin_empirical 2 "empirical" "SModel";
builtin pam 1 "pam" "SModel";
builtin jtt 1 "jtt" "SModel";
builtin wag 1 "wag" "SModel";
builtin lg 1 "lg" "SModel";
builtin fMutSel_q 4 "fMutSel_q" "SModel";
builtin fMutSel_pi 3 "fMutSel_pi" "SModel";
builtin builtin_weighted_frequency_matrix 2 "weighted_frequency_matrix" "SModel";
builtin builtin_frequency_matrix 1 "frequency_matrix" "SModel";
builtin peel_leaf_branch 3 "peel_leaf_branch" "SModel";
builtin alignment_index2 2 "alignment_index2" "SModel";
builtin alignment_index3 3 "alignment_index3" "SModel";
builtin peel_internal_branch 5 "peel_internal_branch" "SModel";
builtin calc_root_probability 5 "calc_root_probability" "SModel";
builtin peel_likelihood_1 3 "peel_likelihood_1" "SModel";
builtin peel_likelihood_2 6 "peel_likelihood_2" "SModel";

data ReversibleMarkov = ReversibleMarkov a b c d e f g;
data ReversibleFrequency = ReversibleFrequency a b c d;
data F81 = F81 a b c d;
data MixtureModel = MixtureModel a;
data MixtureModels = MixtureModels a;

gtr exchange a = builtin_gtr (list_to_vector exchange) a;
equ a = gtr (replicate nn 1.0) a where {n=alphabetSize a;nn=n*(n-1)/2};
tn k1 k2 a = gtr [k1, 1.0, 1.0, 1.0, 1.0, k2] a;
hky k a = tn k k a;

scale x (ReversibleMarkov a s q pi l t r) = ReversibleMarkov a s q pi l (x*t) (x*r);

multiParameter f (DiscreteDistribution d) = MixtureModel (DiscreteDistribution (fmap2 f d));

multi_rate m d = multiParameter (\x->(scale x m)) d;


average_frequency (MixtureModel ms) = list_from_vector $ builtin_average_frequency $ weighted_frequency_matrix $ MixtureModel ms;


extend_mixture (MixtureModel ms) p x = MixtureModel $ extendDiscreteDistribution ms p x;

plus_inv mm p_inv = extend_mixture mm p_inv (scale 0.0 $ f81 pi a) where {a  = getAlphabet mm; pi = average_frequency mm};

multi_rate_unif_bins base dist n_bins = multi_rate base $ uniformDiscretize (quantile dist) n_bins;

rate (ReversibleMarkov a s q pi l t r) = r;
rate (MixtureModel d) = average (fmap2 rate d);

qExp (ReversibleMarkov a s q pi l t r) = lExp l pi t;

branchTransitionP (MixtureModel (DiscreteDistribution l)) t = let {r = rate (MixtureModel (DiscreteDistribution l))} 
                                                              in map (\x -> qExp (scale (t/r) (snd x))) l;

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov' a smap q pi = ReversibleMarkov a smap q pi (get_eigensystem q pi) 1.0 (get_equilibrium_rate a smap q pi);

reversible_markov s (ReversibleFrequency a smap pi r) = reversible_markov' a smap (reversible_rate_matrix s r) pi;

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

weighted_frequency_matrix (MixtureModel d) = let {model = MixtureModel d;
                                                  dist = list_to_vector $ distribution model;
                                                  freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]}
                                             in builtin_weighted_frequency_matrix dist freqs;
weighted_frequency_matrix (MixtureModels (m:ms)) = weighted_frequency_matrix m;

frequency_matrix (MixtureModel d) = let {model = MixtureModel d} 
                                    in  builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1];

frequency_matrix (MixtureModels (m:ms)) = frequency_matrix m;

--
plus_f_equal_frequencies a = plus_f a (replicate n_letters (1.0/intToDouble n_letters)) where {n_letters=alphabetSize a};

jukes_cantor a = reversible_markov (equ a) (plus_f_equal_frequencies a);

k80 kappa nuca = reversible_markov (hky kappa nuca) (plus_f_equal_frequencies nuca);

f81 pi a = reversible_markov (equ a) (plus_f a pi);

m1a_omega_dist f1 w1 = DiscreteDistribution [(f1,w1), (1.0-f1,1.0)];

m2a_omega_dist f1 w1 posP posW = extendDiscreteDistribution (m1a_omega_dist f1 w1) posP posW;

m2a_test_omega_dist f1 w1 posP posW 0 = m2a_omega_dist f1 w1 posP 1.0;
m2a_test_omega_dist f1 w1 posP posW _ = m2a_omega_dist f1 w1 posP posW;

m3_omega_dist ps omegas = DiscreteDistribution $ zip ps omegas;

m3p_omega_dist ps omegas posP posW = extendDiscreteDistribution (m3_omega_dist ps omegas) posP posW;

m3_test_omega_dist ps omegas posP posW 0 = m3p_omega_dist ps omegas posP 1.0;
m3_test_omega_dist ps omegas posP posW _ = m3p_omega_dist ps omegas posP posW;

-- The M7 is just a beta distribution
-- gamma' = var(x)/(mu*(1-mu)) = 1/(a+b+1) = 1/(n+1)
m7_omega_dist mu gamma n_bins = uniformDiscretize (quantile (beta a b)) n_bins where {cap = min (mu/(1.0+mu)) ((1.0-mu)/(2.0-mu));
                                                                                      gamma' = gamma*cap;
                                                                                      n = (1.0/gamma')-1.0;
                                                                                      a = n*mu;
                                                                                      b = n*(1.0 - mu)};

-- The M8 is a beta distribution, where a fraction posP of sites have omega posW
m8_omega_dist mu gamma n_bins posP posW = extendDiscreteDistribution (m7_omega_dist mu gamma n_bins) posP posW;

m8a_omega_dist mu gamma n_bins posP = m8_omega_dist mu gamma n_bins posP 1.0;

m8a_test_omega_dist mu gamma n_bins posP posW 0 = m8_omega_dist mu gamma n_bins posP 1.0;
m8a_test_omega_dist mu gamma n_bins posP posW _ = m8_omega_dist mu gamma n_bins posP posW;

dp_omegas mu omegas = map (\w -> min 1.0 (w*scale)) omegas where {scale = mu/sum(omegas)*(intToDouble $ length omegas)};

dp_omega s r mu omegas codona = multiParameter m0w $ DiscreteDistribution $ zip (repeat p) (dp_omegas mu omegas) where
    {m0w w = reversible_markov (m0 codona s w) r;
     p = 1.0/(intToDouble $ length omegas)};

--  w1 <- uniform 0.0 1.0;
--  [f1, f2] <- dirichlet' 2 1.0;
m1a s r w1 f1 codona = multiParameter m0w (m1a_omega_dist f1 w1)
    where {m0w w = reversible_markov (m0 codona s w) r};

m2a s r w1 f1 posP posW codona = multiParameter m0w (m2a_omega_dist f1 w1 posP posW)
    where {m0w w = reversible_markov (m0 codona s w) r};

m2a_test s r w1 f1 posP posW posSelection codona = multiParameter m0w (m2a_test_omega_dist f1 w1 posP posW posSelection) where {m0w w = reversible_markov (m0 codona s w) r};

m3 s r ps omegas codona = multiParameter m0w (m3_omega_dist ps omegas) where {m0w w = reversible_markov (m0 codona s w) r};

m3_test s r ps omegas posP posW posSelection codona = multiParameter m0w (m3_test_omega_dist ps omegas posP posW posSelection) where {m0w w = reversible_markov (m0 codona s w) r};

m7 s r mu gamma n_bins codona =  multiParameter m0w (m7_omega_dist mu gamma n_bins) where {m0w w = reversible_markov (m0 codona s w) r};

m8 s r mu gamma n_bins posP posW codona = multiParameter m0w (m8_omega_dist mu gamma n_bins posP posW) where {m0w w = reversible_markov (m0 codona s w) r};

m8a s r mu gamma n_bins posP codona = multiParameter m0w (m8a_omega_dist mu gamma n_bins posP) where {m0w w = reversible_markov (m0 codona s w) r};

m8a_test s r mu gamma n_bins posP posW posSelection codona = multiParameter m0w (m8a_test_omega_dist mu gamma n_bins posP posW posSelection) where {m0w w = reversible_markov (m0 codona s w) r};

branch_site s r fs ws posP posW codona = MixtureModels [bg_mixture,fg_mixture] where
    {
-- background omega distribution -- where the last omega is 1.0 (neutral)
      bg_dist = DiscreteDistribution $ zip fs (ws ++ [1.0]);
-- accelerated omega distribution -- posW for all categories
      accel_dist = DiscreteDistribution $ zip fs (repeat posW);
      m0w w = reversible_markov (m0 codona s w) r;
-- background branches always use the background omega distribution              
      bg_mixture = multiParameter m0w (mixDiscreteDistributions [1.0-posP, posP] [bg_dist, bg_dist]);
-- foreground branches use the foreground omega distribution with probability posP
      fg_mixture = multiParameter m0w (mixDiscreteDistributions [1.0-posP, posP] [bg_dist, accel_dist]);
    };

branch_site_test s r fs ws posP posW posSelection codona = branch_site s r fs ws posP posW' codona where
    {posW' = if (posSelection == 1) then posW else 1.0};

frequencies_model a = do {
  let {n_letters = alphabetSize a;
       letters = alphabet_letters a};
  SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     pi <- dirichlet' n_letters 1.0;
     sequence_ $ zipWith (\p l -> Log l p) pi letters;
     return pi
  }
};

pairs l = [(x,y) | (x:ys) <- tails l, y <- ys];

exchange_model a =  do {
  let {lpairs = pairs (alphabet_letters a);
       n = length(lpairs)};
  SamplingRate (1.0/sqrt(intToDouble n)) $ do {
     pi <- dirichlet' n 1.0;
     sequence_ $ zipWith (\p (l1,l2) -> Log (l1++l2) p) pi lpairs;
     return pi
  }
};

get_element_freqs []                 x = error ("No frequency specified for letter '" ++ x ++ "'");
get_element_freqs ((key,value):rest) x = if (key == x) then value else get_element_freqs rest x;

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'");
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y;

constant_frequencies_model freqs a = sequence [get_element_freqs freqs l|l <- alphabet_letters a];

constant_frequencies_model2 freqs a = do {freqs' <- freqs; return [get_element_freqs freqs' l|l <- alphabet_letters a]};

constant_exchange_model ex a = sequence [get_element_exchange ex (l1++l2) (l2++l1)|(l1,l2) <- pairs (alphabet_letters a)];

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

fMutSel_model nuc_rm omega ws codon_a = Prefix "fMutSel" $ do
{
  let {n_letters = alphabetSize codon_a;
       letters = alphabet_letters codon_a;
       nuc_a = getNucleotides codon_a};

  nuc_rm' <- nuc_rm nuc_a;
  
  omega' <- Prefix "omega" omega;
  Log "omega" omega';

  ws' <- SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     ws' <- ws;
     sequence_ $ zipWith (\w l -> Log ("w"++l) w) ws' letters;
     return ws'
  };

  return $ fMutSel codon_a ws' omega' nuc_rm';
};

fMutSel0_model nuc_rm omega ws codon_a = Prefix "fMutSel" $ do
{
  let {aa = getAminoAcids codon_a;
       n_letters = alphabetSize aa;
       letters = alphabet_letters aa;
       nuc_a = getNucleotides codon_a};

  nuc_rm' <- nuc_rm nuc_a;

  omega' <- Prefix "omega" omega;
  Log "omega" omega';

  ws' <- SamplingRate (1.0/sqrt(intToDouble n_letters)) $ do {
     ws' <- ws;
     sequence_ $ zipWith (\w l -> Log ("w"++l) w) ws' letters;
     return ws'
  };

  return $ fMutSel0 codon_a ws' omega' nuc_rm';
};

-- Issue: bad mixing on fMutSel model
-- Issue: how to make M2/M8/branchsite/etc versions of fMutSel model?

-- pi is a vector double here
plus_f_matrix a pi = plus_gwf_matrix a pi 1.0;

-- pi is a vector double here
plus_f' a pi = plus_gwf' a pi 1.0;

plus_f a pi = plus_gwf a pi 1.0;

-- pi is a vector double here
plus_gwf_matrix a pi f = builtin_plus_gwf a f pi;

-- pi is a vector double here
plus_gwf' a pi f = ReversibleFrequency a (simple_smap a) pi (plus_gwf_matrix a pi f);

plus_gwf a pi f = let {pi' = listToVectorDouble pi} in plus_gwf' a pi' f;

f3x4_frequencies a pi1 pi2 pi3 = let {pi1' = listToVectorDouble pi1;
                                      pi2' = listToVectorDouble pi2;
                                      pi3' = listToVectorDouble pi3} in
                                 f3x4_frequencies_builtin a pi1' pi2' pi3';

f1x4_frequencies a pi = let {pi' = listToVectorDouble pi}
                        in f3x4_frequencies_builtin a pi' pi' pi';

f1x4 triplet_a nuc_pi = let {triplet_pi_vec = f1x4_frequencies triplet_a nuc_pi}
                        in  ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (plus_f_matrix triplet_a triplet_pi_vec);

f3x4 triplet_a nuc_pi1 nuc_pi2 nuc_pi3 = let {triplet_pi_vec = f3x4_frequencies triplet_a nuc_pi1 nuc_pi2 nuc_pi3} in
                                         ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (plus_f_matrix triplet_a triplet_pi_vec);

mg94 nuc_pi triplet_a = let {nuc_a          = getNucleotides triplet_a;
                             triplet_pi_vec = f1x4_frequencies triplet_a nuc_pi;
                             nuc_pi_vec     = listToVectorDouble nuc_pi;
                             nuc_r          = plus_f_matrix nuc_a nuc_pi_vec} in
                        ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (muse_gaut_matrix triplet_a nuc_r nuc_r nuc_r);

mg94w9 nuc_pi1 nuc_pi2 nuc_pi3 triplet_a = let {nuc_a          = getNucleotides triplet_a;
                                                triplet_pi_vec = f3x4_frequencies triplet_a nuc_pi1 nuc_pi2 nuc_pi3;
                                                nuc_pi_vec1     = listToVectorDouble nuc_pi1;
                                                nuc_pi_vec2     = listToVectorDouble nuc_pi2;
                                                nuc_pi_vec3     = listToVectorDouble nuc_pi3;
                                                nuc_r1         = plus_f_matrix nuc_a nuc_pi_vec1;
                                                nuc_r2         = plus_f_matrix nuc_a nuc_pi_vec2;
                                                nuc_r3         = plus_f_matrix nuc_a nuc_pi_vec3} in
                                           ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (muse_gaut_matrix triplet_a nuc_r1 nuc_r2 nuc_r3);

gamma_rates_dist alpha = gamma alpha (1.0/alpha);

gamma_rates base alpha n = multi_rate_unif_bins base (gamma_rates_dist alpha) n;

log_normal_rates_dist sigmaOverMu = logNormal lmu lsigma where {x = log(1.0+sigmaOverMu^2);
                                                             lmu = -0.5*x;
                                                             lsigma = sqrt x};

log_normal_rates base sigmaOverMu n = multi_rate_unif_bins base (log_normal_rates_dist sigmaOverMu) n;

dp base rates fraction = multi_rate base (DiscreteDistribution dist) where {dist = zip fraction rates};

branch_transition_p t smodel branch_cat_list ds b = vector_Matrix_From_List $ branchTransitionP (getNthMixture smodel (branch_cat_list!!b)) (ds!b);

transition_p_index t smodel branch_cat_list ds = mkArray (numBranches t) (branch_transition_p t smodel branch_cat_list ds);

a_branch_length_model dist i =
(do {
  t <- dist;
  Log ("*T"++show i) t;
  return t
});

-- If we sample branches that are not adjacent, then it won't be efficient.
iid_branch_length_model t dist = SamplingRate 0.0 $ mapM (\i -> a_branch_length_model dist i) [1..numBranches t];

unit_mixture m = MixtureModel (DiscreteDistribution [(1.0,m)]);

mmm m = MixtureModels [m];

empirical a filename = builtin_empirical a (listToString filename);

cached_conditional_likelihoods t seqs as alpha ps f = let {lc    = mkArray (2*numBranches t) lcf;
                                                           lcf b = let {bb = b `mod` (numBranches t)} in
                                                                   case edgesBeforeEdge t b of {
                                                                       []      -> peel_leaf_branch (seqs!sourceNode t b) alpha (ps!bb);
                                                                       [b1,b2] -> peel_internal_branch (lc!b1) (lc!b2) (alignment_index2 (as!b1) (as!b2)) (ps!bb) f}
                                                          }
                                                      in lc;

peel_likelihood t cl as f root = let {branches_in = map (reverseEdge t) (edgesOutOfNode t root);} in
                                 case branches_in of {[b1,b2,b3]->
                                                      calc_root_probability (cl!b1) (cl!b2) (cl!b3) (alignment_index3 (as!b1) (as!b2) (as!b3)) f};
}
