module Parameters where  
{
import Range;

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

findBinary' p = [p2 | (p2,TrueFalseRange) <- findAtomic p];
findBinary = listToVectorInt . (map getModifiableIndex).findBinary';

getModifiableIndex x = x;
}