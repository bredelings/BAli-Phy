module Test where
f r x = if null x then r else tail x
g x r = if null x then r else tail x
h x y = fst (if x then (1,y) else (y,3))
q y x = fst (if x then (1,y) else (y,3))
