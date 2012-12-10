DeclareParameter "x"
x = 2.0
DeclareParameter "y"
y = 3.0

x ~ Normal(0.0, 1.0)
y ~ Normal(x, 1.0)

DeclareParameter "p"
p ~ Beta(10.0, 1.0)
VarBounds p 0.0 1.0
p = 0.5
MakeLogger p

DeclareParameter "z"
z ~ Mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ])
z = 2.5

DeclareParameter "w"
w ~ Mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ])
w = 0.0
MakeLogger w

DeclareParameter "x1"
DeclareParameter "x2"
[x1,x2] ~ iid (2,normal (1.0, 1.0))
x1 = 1.0
x2 = 2.0
MakeLogger x1
MakeLogger x2

DeclareParameter "theta"
theta ~ Exponential(1.0)
theta = 1.0
VarBounds theta 0.0 false
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afsGroup (theta)
MakeLogger theta

DeclareParameter "theta1"
DeclareParameter "theta2"
theta1 ~ Exponential(1.0)
theta1 = 1.0
theta2 ~ Exponential(1.0)
theta2 = 1.0
DeclareParameter "i"
i ~ bernoulli(0.5)
i = false
DeclareParameter "p2"
p2 ~ Beta(1.0, 1.0)
p2 = 0.5
VarBounds p2 0.0 1.0
VarBounds theta1 0.0 false
VarBounds theta2 0.0 false
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afsMixture ([theta1,theta2],if i then [p2,1.0-p2] else [1.0,0.0])
MakeLogger theta1
MakeLogger theta2
MakeLogger p2
MakeLogger i
