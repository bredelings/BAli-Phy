module Test where {
-- straightforward code for
-- f (x:xs) (y:ys) = xs++ys
-- f (x:xs) []     = [x]
-- f _      _      = error "f"
f a b = let {fail1 = let {fail2 = error "f"} in case a of {[]->fail2; x:xs-> case b of {[]->[x]; y:ys -> fail2}}} in
        case a of {_ -> fail1; x:xs  -> case b of {_ -> fail1; y:ys -> xs++ys}};

}
