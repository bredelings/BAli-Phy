-- straightforward code for
-- f (x:xs) (y:ys) = xs++ys
-- f (x:xs) []     = [x]
-- f _      _      = error "f"
f a b = let {fail1 = let {fail2 = error "f"} in case a of {[]->fail2; x:xs-> case b of {[]->[x]; y:ys -> fail2}}} in
        case a of {x:xs  -> case b of {y:ys -> xs++ys;_ -> fail1};_ -> fail1};

f2 (x:xs) (y:ys) = xs++ys
f2 (x:xs) []     = [x]
f2 _      _      = error "f"

f3 (x:xs) (y:ys) = xs++ys
f3 (x:xs) []     = [x]
