module Test where {
f x = let {compose f g x = f (g x); square x = x*x;} in compose square (+2) x;
}
