module Test where {
f x = case x of {x:xs -> x;[]->error "yo!"};
g x = 10 + f x;
h = g [2];
}
