module Parameters where  
{
import Range;
import Distributions;

builtin builtin_set_modifiable_value 3 "set_modifiable_value" "Modifiables";
builtin is_changeable 1 "is_changeable" "Modifiables";
builtin is_modifiable 1 "is_modifiable" "Modifiables";
builtin get_modifiable_index 1 "get_modifiable_index" "Modifiables";
builtin builtin_new_modifiable 1 "new_modifiable" "Modifiables";
builtin builtin_new_random_modifiable 1 "new_random_modifiable" "Modifiables";
builtin evaluate 2 "evaluate";
builtin get_modifiable_value 2 "get_modifiable_value" "Modifiables";
builtin builtin_trigger 1 "trigger";

new_random_modifiable dist = IOAction1 builtin_new_random_modifiable dist;

new_modifiable = IOAction1 builtin_new_modifiable ();

new_modifiable_list [] = return [];
new_modifiable_list (h:t) = do { m <- h; 
                                 ms <- new_modifiable_list t; 
                                 return (m:ms) 
                               };

structure_for_dist dist = let {r = distRange dist} in
                          case r of {OpenInterval _ _ -> new_random_modifiable r;
                                     IntegerInterval _ _ -> new_random_modifiable r;
                                     TrueFalseRange -> new_random_modifiable r;
                                     Simplex n _ -> new_modifiable_list (replicate n new_modifiable);
                                     ListRange _ -> sample' dist};

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

subscript name n = name++['!']++show n;

subscripts name xs = map (subscript name) xs;

find_loggables' ([],name) = [];
find_loggables' (p:ps,names) = concat $ map find_loggables (zip (p:ps) (subscripts names [0..]));

find_loggables (p,name) = if (is_modifiable p)
                                then [(p,name)]
                                else find_loggables' (p,name);
  
find_loggables_c p name = list_to_vector [c_pair (get_modifiable_index m, listToString name) | (m,name) <-find_loggables (p,name)  ];

findAtomic ps (ListRange rs) = concat $ zipWith findAtomic ps rs;
findAtomic p r = [(p,r)];

findBinary p r = list_to_vector [get_modifiable_index m | (m,TrueFalseRange) <- findAtomic p r];

findReal p r = list_to_vector [c_pair (get_modifiable_index m, getBounds (OpenInterval x y)) 
                              | (m,OpenInterval x y) <- findAtomic p r];

findBoundedInteger p r = list_to_vector [c_pair (get_modifiable_index m, c_pair (x,y))
                                          | (m,IntegerInterval (Just x) (Just y)) <- findAtomic p r];

findInteger p r = list_to_vector [c_pair (get_modifiable_index m, getIntegerBounds (IntegerInterval x y)) 
                                   | (m, IntegerInterval x y) <- findAtomic p r];

findSimplex p r = list_to_vector [c_pair (list_to_vector $ map get_modifiable_index ms, c_pair (n,total)) 
                                 | (ms,Simplex n total) <- findAtomic p r];
  
trigger i = IOAction1 builtin_trigger i;

trigger_on x i = unsafePerformIO $ do {return x;trigger i};
}
