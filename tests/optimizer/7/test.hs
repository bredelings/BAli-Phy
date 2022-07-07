module Test where {
f x = let {h x y = g y; g y = k y; k y = h (1::Int) y} in g x;
}
