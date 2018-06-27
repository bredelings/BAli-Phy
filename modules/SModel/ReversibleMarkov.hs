module SModel.ReversibleMarkov where
{
builtin get_equilibrium_rate 4 "get_equilibrium_rate" "SModel";
builtin get_eigensystem 2 "get_eigensystem" "SModel";
builtin reversible_rate_matrix 2 "reversible_rate_matrix" "SModel";
builtin lExp 3 "lExp" "SModel";

data ReversibleFrequency = ReversibleFrequency a b c d;
data ReversibleMarkov = ReversibleMarkov a b c d e f g;

qExp (ReversibleMarkov a s q pi l t r) = lExp l pi t;

scale x (ReversibleMarkov a s q pi l t r) = ReversibleMarkov a s q pi l (x*t) (x*r);

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov' a smap q pi = ReversibleMarkov a smap q pi (get_eigensystem q pi) 1.0 (get_equilibrium_rate a smap q pi);

reversible_markov s (ReversibleFrequency a smap pi r) = reversible_markov' a smap (reversible_rate_matrix s r) pi;

}
