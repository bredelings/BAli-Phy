DeclareParameter "x"
x = 2.0
DeclareParameter "y"
y = 3.0
x ~ Normal(0.0, 1.0)
y ~ Normal(x, 1.0)
