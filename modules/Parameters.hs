module Parameters where  
{
import Range;
import Distributions;

builtin builtin_set_modifiable_value 3 "set_modifiable_value" "Modifiables";
builtin is_changeable 1 "is_changeable" "Modifiables";
builtin is_modifiable 1 "is_modifiable" "Modifiables";
builtin get_modifiable_index 1 "get_modifiable_index" "Modifiables";
builtin builtin_new_modifiable 1 "new_modifiable" "Modifiables";
builtin builtin_new_random_modifiable 3 "new_random_modifiable" "Modifiables";
builtin evaluate 2 "evaluate";
builtin get_modifiable_value 2 "get_modifiable_value" "Modifiables";
builtin builtin_trigger 1 "trigger";
builtin builtin_add_parameter 2 "add_parameter" "Modifiables";
builtin builtin_register_probability 1 "register_probability" "Modifiables";

add_parameter name x = IOAction2 builtin_add_parameter (listToString name) x;

register_probability pr = IOAction1 builtin_register_probability pr;

new_random_modifiable range value = IOAction3 builtin_new_random_modifiable range (c_range range) value;

c_range (OpenInterval a b) = getBounds (OpenInterval a b);
c_range (IntegerInterval a b) = getIntegerBounds (IntegerInterval a b);
c_range r = r;

new_modifiable = IOAction1 builtin_new_modifiable ();

new_modifiable_list [] = return [];
new_modifiable_list (h:t) = do { m <- h; 
                                 ms <- new_modifiable_list t; 
                                 return (m:ms) 
                               };

structure_for_dist = sample';

set_modifiable_value token m v = IOAction3 builtin_set_modifiable_value token m v;

set_parameter_value' token (p:ps) (v:vs) = do { set_parameter_value token p v; 
                                                set_parameter_value token ps vs
                                              };
set_parameter_value' token [] [] = return ();  

set_parameter_value token p v = if (is_modifiable p) 
                                then set_modifiable_value token p v 
                                else set_parameter_value' token p v;

set_parameter_value_' token (p:ps) (v:vs) = do { set_parameter_value_ token p (evaluate token v); 
                                                 set_parameter_value_ token ps (evaluate token vs)
                                              };
set_parameter_value_' token [] [] = return ();  

set_parameter_value_ token p v = if (is_modifiable p) 
                                 then set_modifiable_value token p v
                                 else set_parameter_value_' token p v;

get_modifiable_result token m = evaluate token (get_modifiable_value token m);

trigger i = IOAction1 builtin_trigger i;

trigger_on x i = unsafePerformIO $ do {return x;trigger i};
}
