module SModel.ReversibleMarkov (module SModel.ReversibleMarkov, module SModel.ReversibleFrequency) where
{
import SModel.ReversibleFrequency;
import Alphabet;

builtin get_equilibrium_rate 4 "get_equilibrium_rate" "SModel";
builtin get_eigensystem 2 "get_eigensystem" "SModel";
builtin lExp 3 "lExp" "SModel";
builtin builtin_gtr_sym 2 "gtr_sym" "SModel";
builtin fixup_diagonal_rates 1 "fixup_diagonal_rates" "SModel";
builtin %*% 2 "elementwise_multiply" "SModel";

data ReversibleMarkov = ReversibleMarkov a b c d e f g;

qExp (ReversibleMarkov a s q pi l t r) = lExp l pi t;

get_q (ReversibleMarkov _ _ q _ _ _ _) = q;

scale x (ReversibleMarkov a s q pi l t r) = ReversibleMarkov a s q pi l (x*t) (x*r);

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov' a smap q pi = ReversibleMarkov a smap q2 pi (get_eigensystem q2 pi) 1.0 (get_equilibrium_rate a smap q2 pi) where {q2 = fixup_diagonal_rates q};

reversible_markov s (ReversibleFrequency a smap pi r) = reversible_markov' a smap (s %*% r) pi;

gtr_sym exchange a = builtin_gtr_sym (list_to_vector exchange) a;
equ a = gtr_sym (replicate nn 1.0) a where {n=alphabetSize a;nn=n*(n-1)/2};

jukes_cantor a = reversible_markov (equ a) (plus_f_equal_frequencies a);
f81        pi  a = reversible_markov (equ            a) (plus_f a pi);
gtr s      pi  a = reversible_markov (gtr_sym s      a) (plus_f a pi);

gtr' s'    pi' a = reversible_markov (gtr_sym' s'    a) (plus_f' a pi');

-- es' is a [(String,Double)] here
all_pairs l = [(x,y) | (x:ys) <- tails l, y <- ys];

letter_pair_names a = [l1++l2|(l1,l2) <- all_pairs (alphabet_letters a)];

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'");
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y;

gtr_sym' es' a = gtr_sym es a where {lpairs = all_pairs (alphabet_letters a);
                                     es = if length lpairs == length es' then
                                              [get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                          else
                                              error "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"};

plus_f''   a s pi = reversible_markov s (plus_f' a pi);
plus_fe''   a s = reversible_markov s (plus_f a (uniform_frequencies a));
plus_gwf'' a s pi f = reversible_markov s (plus_gwf' a pi f);
}
