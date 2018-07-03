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

}
