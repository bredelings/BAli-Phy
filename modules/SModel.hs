module SModel (module SModel, module SModel.ReversibleMarkov) where 
{
import Distributions;
import Alphabet;
import Tree;
import Parameters;

import SModel.ReversibleMarkov;

builtin f3x4_frequencies_builtin 4 "f3x4_frequencies" "SModel";
builtin muse_gaut_matrix 4 "muse_gaut_matrix" "SModel";
builtin builtin_plus_gwf 3 "plus_gwF" "SModel";
builtin builtin_average_frequency 1 "average_frequency" "SModel";
builtin builtin_gtr_sym 2 "gtr_sym" "SModel";
builtin m0 3 "m0" "SModel";
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

-- peeling for connected-CLVs
builtin peel_leaf_branch 4 "peel_leaf_branch" "SModel";
builtin alignment_index2 2 "alignment_index2" "SModel";
builtin alignment_index3 3 "alignment_index3" "SModel";
builtin peel_internal_branch 6 "peel_internal_branch" "SModel";
builtin calc_root_probability 7 "calc_root_probability" "SModel";

-- peeling for SEV
builtin bitmask_from_alignment 2 "bitmask_from_alignment" "Alignment";
builtin peel_leaf_branch_SEV 4 "peel_leaf_branch_SEV" "SModel";
builtin peel_internal_branch_SEV 4 "peel_internal_branch_SEV" "SModel";
builtin calc_root_probability_SEV 5 "calc_root_probability_SEV" "SModel";

builtin peel_likelihood_1 3 "peel_likelihood_1" "SModel";
builtin peel_likelihood_2 6 "peel_likelihood_2" "SModel";

data F81 = F81 a b c d;
data MixtureModel = MixtureModel a;
data MixtureModels = MixtureModels a;

gtr_sym exchange a = builtin_gtr_sym (list_to_vector exchange) a;
equ a = gtr_sym (replicate nn 1.0) a where {n=alphabetSize a;nn=n*(n-1)/2};
tn93_sym k1 k2 a = gtr_sym [1.0, k1, 1.0, 1.0, k2, 1.0] a;
hky85_sym k a = tn93_sym k k a;

scaleMM x (MixtureModel dist            ) = MixtureModel [(p, scale x m) | (p, m) <- dist];

mixMM fs ms = MixtureModel $ mix fs [m | MixtureModel m <- ms];
scale_MMs rs ms = [scaleMM r m | (r,m) <- zip' rs ms];
scaled_mixture ms rs fs = mixMM fs (scale_MMs rs ms);
scaled_mixture' a ms rs fs = scaled_mixture (map ($a) ms) rs fs;

multiParameter model_fn values = MixtureModel [ (f*p, m) |(p,x) <- values, let {dist = case model_fn x of {MixtureModel d -> d}}, (f,m) <- dist];

multi_rate m d = multiParameter (\x->scaleMM x m) d;

average_frequency (MixtureModel ms) = list_from_vector $ builtin_average_frequency $ weighted_frequency_matrix $ MixtureModel ms;

extend_mixture (MixtureModel ms) (p,x) = MixtureModel $ mix [p, 1.0-p] [certainly x, ms];

plus_inv mm p_inv = extend_mixture mm (p_inv, scale 0.0 $ f81 pi a) where {a  = getAlphabet mm; pi = average_frequency mm};

multi_rate_unif_bins base dist n_bins = multi_rate base $ uniformDiscretize dist n_bins;

rate (ReversibleMarkov a s q pi l t r) = r;
rate (MixtureModel d) = average (fmap2 rate d);

branchTransitionP (MixtureModel l) t = let {r = rate (MixtureModel l)} 
                                                              in map (\x -> qExp (scale (t/r) (snd x))) l;

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
nBaseModels (MixtureModel l) = length l;
nBaseModels (MixtureModels (m:ms)) = nBaseModels m;

baseModel (MixtureModel l) i = snd (l !! i);

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

distribution (MixtureModel l) = map fst l;
distribution (MixtureModels (m:ms))                  = distribution m;

getNthMixture (MixtureModels l) i = l !! i;

unwrapMM (MixtureModel dd) = dd;

mixMixtureModels l dd = MixtureModel (mix l (map unwrapMM dd));

weighted_frequency_matrix (MixtureModel d) = let {model = MixtureModel d;
                                                  dist = list_to_vector $ distribution model;
                                                  freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]}
                                             in builtin_weighted_frequency_matrix dist freqs;
weighted_frequency_matrix (MixtureModels (m:ms)) = weighted_frequency_matrix m;

frequency_matrix (MixtureModel d) = let {model = MixtureModel d} 
                                    in  builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1];

frequency_matrix (MixtureModels (m:ms)) = frequency_matrix m;

--
uniform_frequencies a = zip letters (repeat $ 1.0/(intToDouble n_letters)) where {letters = alphabet_letters a;
                                                                                  n_letters = alphabetSize a};

plus_f_equal_frequencies a = plus_f a (replicate n_letters (1.0/intToDouble n_letters)) where {n_letters=alphabetSize a};

jukes_cantor a = reversible_markov (equ a) (plus_f_equal_frequencies a);

k80 kappa nuca = reversible_markov (hky85_sym kappa nuca) (plus_f_equal_frequencies nuca);

f81        pi a = reversible_markov (equ            a) (plus_f a pi);
hky85 k    pi a = reversible_markov (hky85_sym k    a) (plus_f a pi);
tn93 k1 k2 pi a = reversible_markov (tn93_sym k1 k2 a) (plus_f a pi);
gtr s      pi a = reversible_markov (gtr_sym s      a) (plus_f a pi);

hky85' k    pi' a = reversible_markov (hky85_sym k    a) (plus_f' a pi');
tn93' k1 k2 pi' a = reversible_markov (tn93_sym k1 k2 a) (plus_f' a pi');
gtr' s'     pi' a = reversible_markov (gtr_sym' s'    a) (plus_f' a pi');

m1a_omega_dist f1 w1 = [(f1,w1), (1.0-f1,1.0)];

m2a_omega_dist f1 w1 posP posW = extendDiscreteDistribution (m1a_omega_dist f1 w1) posP posW;

m2a_test_omega_dist f1 w1 posP posW 0 = m2a_omega_dist f1 w1 posP 1.0;
m2a_test_omega_dist f1 w1 posP posW _ = m2a_omega_dist f1 w1 posP posW;

m3_omega_dist ps omegas = zip' ps omegas;

m3p_omega_dist ps omegas posP posW = extendDiscreteDistribution (m3_omega_dist ps omegas) posP posW;

m3_test_omega_dist ps omegas posP posW 0 = m3p_omega_dist ps omegas posP 1.0;
m3_test_omega_dist ps omegas posP posW _ = m3p_omega_dist ps omegas posP posW;

-- The M7 is just a beta distribution
-- gamma' = var(x)/(mu*(1-mu)) = 1/(a+b+1) = 1/(n+1)
m7_omega_dist mu gamma n_bins = uniformDiscretize (beta a b) n_bins where {cap = min (mu/(1.0+mu)) ((1.0-mu)/(2.0-mu));
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

omega_mixture codona s r dist = multiParameter (\w -> unit_mixture $ reversible_markov (m0 codona s w) r) dist;

dp_omega s r mu omegas codona = omega_mixture codona s r $ zip (repeat p) (dp_omegas mu omegas) where
    {p = 1.0/(intToDouble $ length omegas)};

--  w1 <- uniform 0.0 1.0;
--  [f1, f2] <- dirichlet' 2 1.0;
m1a s r w1 f1 codona = omega_mixture codona s r (m1a_omega_dist f1 w1);

m2a s r w1 f1 posP posW codona = omega_mixture codona s r (m2a_omega_dist f1 w1 posP posW);

m2a_test s r w1 f1 posP posW posSelection codona = omega_mixture codona s r (m2a_test_omega_dist f1 w1 posP posW posSelection);

m3 s r ps omegas codona = omega_mixture codona s r (m3_omega_dist ps omegas);

m3_test s r ps omegas posP posW posSelection codona = omega_mixture codona s r (m3_test_omega_dist ps omegas posP posW posSelection);

m7 s r mu gamma n_bins codona =  omega_mixture codona s r (m7_omega_dist mu gamma n_bins);

m8 s r mu gamma n_bins posP posW codona = omega_mixture codona s r (m8_omega_dist mu gamma n_bins posP posW);

m8a s r mu gamma n_bins posP codona = omega_mixture codona s r (m8a_omega_dist mu gamma n_bins posP);

m8a_test s r mu gamma n_bins posP posW posSelection codona = omega_mixture codona s r (m8a_test_omega_dist mu gamma n_bins posP posW posSelection);

branch_site s r fs ws posP posW codona = MixtureModels [bg_mixture,fg_mixture] where
    {
-- background omega distribution -- where the last omega is 1.0 (neutral)
      bg_dist = zip fs (ws ++ [1.0]);
-- accelerated omega distribution -- posW for all categories
      accel_dist = zip fs (repeat posW);
-- background branches always use the background omega distribution              
      bg_mixture = omega_mixture codona s r (mix [1.0-posP, posP] [bg_dist, bg_dist]);
-- foreground branches use the foreground omega distribution with probability posP
      fg_mixture = omega_mixture codona s r (mix [1.0-posP, posP] [bg_dist, accel_dist]);
    };

branch_site_test s r fs ws posP posW posSelection codona = branch_site s r fs ws posP posW' codona where
    {posW' = if (posSelection == 1) then posW else 1.0};

all_pairs l = [(x,y) | (x:ys) <- tails l, y <- ys];

letter_pair_names a = [l1++l2|(l1,l2) <- all_pairs (alphabet_letters a)];

get_element_freqs []                 x = error ("No frequency specified for letter '" ++ x ++ "'");
get_element_freqs ((key,value):rest) x = if (key == x) then value else get_element_freqs rest x;

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'");
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y;

select_element x ((key,value):rest) = if x == key then value else select_element x rest;
select_element x [] = error $ "Can't find element " ++ show x ++ " in dictionary!";

select_elements (x:xs) pi = select_element x pi:select_elements xs pi;
select_elements []      _ = [];

get_ordered_elements xs xps plural = if length xs == length xps then select_elements xs xps else error $ "Expected "++show (length xs)++" "++plural
                                     ++" but got "++ show (length xps)++"!";

gtr_sym' es' a = gtr_sym es a where {lpairs = all_pairs (alphabet_letters a);
                                     es = if length lpairs == length es' then
                                              [get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                          else
                                              error "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"};

plus_f' a pi' = plus_f a pi where {pi= get_ordered_elements (alphabet_letters a) pi' "frequencies"};

plus_gwf' a pi' f = plus_gwf a pi f where {pi= get_ordered_elements (alphabet_letters a) pi' "frequencies"};

f1x4' a pi' = f1x4 a pi where {nuc_letters = alphabet_letters (getNucleotides a);
                               pi = get_ordered_elements nuc_letters pi' "frequencies"};

f3x4' a pi1' pi2' pi3' = f3x4 a pi1 pi2 pi3 where {nuc_letters = alphabet_letters (getNucleotides a);
                                                   pi1 = get_ordered_elements nuc_letters pi1' "frequencies";
                                                   pi2 = get_ordered_elements nuc_letters pi2' "frequencies";
                                                   pi3 = get_ordered_elements nuc_letters pi3' "frequencies"};

mg94' pi' a = mg94 pi a  where {nuc_letters = alphabet_letters (getNucleotides a);
                                pi = get_ordered_elements nuc_letters pi' "frequencies"};

mg94w9' pi1' pi2' pi3' a = mg94w9 pi1 pi2 pi3 a where {nuc_letters = alphabet_letters (getNucleotides a);
                                                       pi1 = get_ordered_elements nuc_letters pi1' "frequencies";
                                                       pi2 = get_ordered_elements nuc_letters pi2' "frequencies";
                                                       pi3 = get_ordered_elements nuc_letters pi3' "frequencies"};

simple_smap a = iotaUnsigned (alphabetSize a);

fMutSel codon_a codon_w omega (ReversibleMarkov _ _ nuc_q nuc_pi _ _ _) =
   let {nuc_a = getNucleotides codon_a;
        smap = simple_smap codon_a;
        codon_w' = listToVectorDouble codon_w;
        q  = fMutSel_q  codon_a codon_w' omega nuc_q;
        pi = fMutSel_pi codon_a codon_w' nuc_pi}
   in reversible_markov' codon_a smap q pi;

fMutSel' codon_a codon_ws' omega nuc_model = fMutSel codon_a codon_ws omega nuc_model
                                               where {codon_ws = get_ordered_elements (alphabet_letters codon_a) codon_ws' "fitnesses"};

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 a w omega nuc_q  = fMutSel a codon_w omega nuc_q
--replicate n_letters (1.0/intToDouble n_letters);
where {codon_w = [w'!aa| codon <- codons,let {aa = translate a codon}];
         w' = listArray' w;
         codons = take n_letters [0..];
         n_letters = alphabetSize a};

fMutSel0' codon_a amino_ws' omega nuc_model = fMutSel0 codon_a amino_ws omega nuc_model
                                               where {amino_ws = get_ordered_elements (alphabet_letters amino_a) amino_ws' "fitnesses";
                                                      amino_a = getAminoAcids codon_a};

-- Issue: bad mixing on fMutSel model
-- Issue: how to make M2/M8/branchsite/etc versions of fMutSel model?

-- pi is a vector double here
plus_f_matrix a pi = plus_gwf_matrix a pi 1.0;

-- pi is a vector double here
plus_gwf_matrix a pi f = builtin_plus_gwf a f pi;

-- pi is [Double] here
plus_gwf a pi f = ReversibleFrequency a (simple_smap a) pi' (plus_gwf_matrix a pi' f) where {pi' = listToVectorDouble pi};
plus_f a pi = plus_gwf a pi 1.0;

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

log_normal_rates_dist sigmaOverMu = log_normal lmu lsigma where {x = log(1.0+sigmaOverMu^2);
                                                             lmu = -0.5*x;
                                                             lsigma = sqrt x};

log_normal_rates base sigmaOverMu n = multi_rate_unif_bins base (log_normal_rates_dist sigmaOverMu) n;

--dp base rates fraction = multi_rate base dist where {dist = zip fraction rates};
dp base rates fraction = scaled_mixture (replicate (length fraction) base) rates fraction;

branch_transition_p t smodel branch_cat_list ds b = list_to_vector $ branchTransitionP (getNthMixture smodel (branch_cat_list!!b)) (ds!b);

transition_p_index t smodel branch_cat_list ds = mkArray (numBranches t) (branch_transition_p t smodel branch_cat_list ds);

unit_mixture m = MixtureModel (certainly m);

mmm m = MixtureModels [m];

empirical a filename = builtin_empirical a (listToString filename);

cached_conditional_likelihoods t seqs counts as alpha ps f = let {lc    = mkArray (2*numBranches t) lcf;
                                                                  lcf b = let {bb = b `mod` (numBranches t)} in
                                                                          case edgesBeforeEdge t b of {
                                                                              []      -> let {n=sourceNode t b}
                                                                                         in peel_leaf_branch (seqs!n) (counts!n) alpha (ps!bb);
                                                                              [b1,b2] -> peel_internal_branch (lc!b1) (lc!b2) (as!b1) (as!b2) (ps!bb) f}
                                                                 }
                                                             in lc;

peel_likelihood t cl as f root = let {likelihoods = mkArray (numNodes t) peel_likelihood';
                                      peel_likelihood' root = let {branches_in = map (reverseEdge t) (edgesOutOfNode t root);} in
                                                              case branches_in of {[b1,b2,b3]-> calc_root_probability (cl!b1) (cl!b2) (cl!b3) (as!b1) (as!b2) (as!b3) f};
                                     }
                                 in likelihoods!root;


cached_conditional_likelihoods_SEV t seqs alpha ps f a =
    let {lc    = mkArray (2*numBranches t) lcf;
                 lcf b = let {bb = b `mod` (numBranches t)} in
                         case edgesBeforeEdge t b of {
                                                    []      -> peel_leaf_branch_SEV (seqs!sourceNode t b) alpha (ps!bb) (bitmask_from_alignment a $ sourceNode t b);
                                                    [b1,b2] -> peel_internal_branch_SEV (lc!b1) (lc!b2) (ps!bb) f}
        }
    in lc;

peel_likelihood_SEV t cl f root counts = let {branches_in = map (reverseEdge t) (edgesOutOfNode t root);} in
                                         case branches_in of {[b1,b2,b3]-> calc_root_probability_SEV (cl!b1) (cl!b2) (cl!b3) f counts};
}
