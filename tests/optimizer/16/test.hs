module Test where {
data Either a b = Left a | Right b;
f x = case x of {Left a -> 2;Right b -> 2};
}
