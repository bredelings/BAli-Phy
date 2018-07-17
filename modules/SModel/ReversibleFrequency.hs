module SModel.ReversibleFrequency where
{
import Alphabet;
data ReversibleFrequency = ReversibleFrequency a b c d;
builtin builtin_plus_gwf 3 "plus_gwF" "SModel";

simple_smap a = iotaUnsigned (alphabetSize a);

-- pi is a vector double here
plus_f_matrix a pi = plus_gwf_matrix a pi 1.0;

-- pi is a vector double here
plus_gwf_matrix a pi f = builtin_plus_gwf a f pi;

-- pi is [Double] here
plus_gwf a pi f = ReversibleFrequency a (simple_smap a) pi' (plus_gwf_matrix a pi' f) where {pi' = listToVectorDouble pi};
plus_f a pi = plus_gwf a pi 1.0;

uniform_frequencies a = zip letters (repeat $ 1.0/(intToDouble n_letters)) where {letters = alphabet_letters a;
                                                                                  n_letters = alphabetSize a};

plus_f_equal_frequencies a = plus_f a (replicate n_letters (1.0/intToDouble n_letters)) where {n_letters=alphabetSize a};

-- pi is [(String,Double)] here
select_element x ((key,value):rest) = if x == key then value else select_element x rest;
select_element x [] = error $ "Can't find element " ++ show x ++ " in dictionary!";

select_elements (x:xs) pi = select_element x pi:select_elements xs pi;
select_elements []      _ = [];

get_ordered_elements xs xps plural = if length xs == length xps then select_elements xs xps else error $ "Expected "++show (length xs)++" "++plural
                                     ++" but got "++ show (length xps)++"!";
plus_f' a pi' = plus_f a pi where {pi= get_ordered_elements (alphabet_letters a) pi' "frequencies"};
plus_gwf' a pi' f = plus_gwf a pi f where {pi= get_ordered_elements (alphabet_letters a) pi' "frequencies"};

}
