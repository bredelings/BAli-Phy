module Parameters where  
{
import Range;

builtin builtin_set_modifiable_value 3 "set_modifiable_value";
builtin is_changeable 2 "is_changeable";
builtin is_modifiable 2 "is_modifiable";
builtin get_modifiable_index 2 "get_modifiable_index";
builtin get_modifiable_for_index 2 "get_modifiable_for_index";

set_modifiable_value token m v = IOAction3 builtin_set_modifiable_value token m v;
set_parameter_value' token (p:ps) (v:vs) = do { set_parameter_value token p v; 
                                                set_parameter_value token ps vs
                                              };
set_parameter_value' token [] [] = return ();  

set_parameter_value token p v = if (is_modifiable token p) 
                                then set_modifiable_value token p v 
                                else set_parameter_value' token p v;

findAtomic ps (ListRange rs) = concat $ zipWith findAtomic ps rs;
findAtomic p r = [(p,r)];

findBinary t p r = list_to_vector [get_modifiable_index t m | (m,TrueFalseRange) <- findAtomic p r];

findReal t p r = list_to_vector [c_pair (get_modifiable_index t m, getBounds (OpenInterval x y)) 
                              | (m,OpenInterval x y) <- findAtomic p r];

findInteger' p r = [p2 | (p2,IntegerInterval _ _) <- findAtomic p r];
findInteger = listToVectorInt . (map get_modifiable_index) . findInteger';

findSimplex' p r = [p2 | (p2,Simplex _ _) <- findAtomic p r];
findSimplex = listToVectorInt . (map get_modifiable_index) . findSimplex';
}