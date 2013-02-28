module Parameters where  
{
import Range;

builtin builtin_set_modifiable_value 3 "set_modifiable_value";
builtin is_changeable 2 "is_changeable";
builtin is_modifiable 2 "is_modifiable";
builtin get_modifiable_index 2 "get_modifiable_index";
builtin get_modifiable_for_index 2 "get_modifiable_for_index";
builtin builtin_new_modifiable 1 "new_modifiable";

new_modifiable token = IOAction3 builtin_new_modifiable token;

new_modifiable_list [] token = return [];
new_modifiable_list (h:t) token = do { m <- h token; 
                                       ms <- t token; 
                                       return (m:ms) 
                                     };

structure_for_range (OpenInterval _ _) = new_modifiable;
structure_for_range (IntegerInterval _ _) = new_modifiable;
structure_for_range TrueFalseRange = new_modifiable;
structure_for_range (Simplex n _) = new_modifiable_list (replicate n new_modifiable);
structure_for_range (ListRange l) = new_modifiable_list (map structure_for_range l);

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

findInteger t p r = list_to_vector [c_pair (get_modifiable_index t m, getBounds (IntegerInterval x y)) 
                              | (m,IntegerInterval x y) <- findAtomic p r];

findSimplex t p r = list_to_vector [c_pair (list_to_vector $ map (get_modifiable_index t) ms, c_pair (n,total)) 
                                 | (ms,Simplex n total) <- findAtomic p r];
}