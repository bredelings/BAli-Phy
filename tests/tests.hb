DeclareParameter "x"
x = 2
DeclareParameter "y"
x ~ Normal()
y ~ Normal(x,1)
y = 3
"\""
'"'
'j'
"o"
x ~ Normal(0.0)
x ~ Normal(0.0,0.00)
x ~ Normal(0.0 , 0.00)
x ~ Normal(0,1)
0:0:[]
0+(0+0)
0==(0+0)
(==)
"1"
0+0-0
0+0-(-0)
x*y+z*w
0 0
[x]
[x,x]
(x,x)
N.x
(x)
x (z y)
x z y
(+) + (*)
fmap (+)
fmap (1+)
x ++ z ++ y
x `seq` z `seq` y
(,,,) z (z y)
Maybe.mayb2e z (z y)
Mayb2e z (z y)
Maybe.Mayb2e z (z y)
fmap (\x->x+1) y
if x then y else z
\x->if x then y else z
\x y->if x then y else z
(x,y) ~ Normal(x)
[x,y] ~ Normal(x)
findFirst (\n->(targetNode t n)==n2) (edgesOutOfNode t n1)
[edgeForNodes (n,n1) | n <- neighbors t n1, n /= n2]
let {x = x; f y = y} in x
case x of {(y,_)-> y}
case x of {[y,_]-> y}
case x of {h:t-> h}
case x of {1-> h}
case x of {-1-> h}
case x of {1.0 -> h}
case x of {-1.0-> h}
case x of {Maybe h _ -> h}
let {edgeForNodes t (n1,n2) = findFirst (\n->(targetNode t n)==n2) (edgesOutOfNode t n1)} in x
let {x=2;edgeForNodes t (n1,n2) = findFirst (\n->(targetNode t n)==n2) (edgesOutOfNode t n1)} in x
