DeclareParameter "x"
x = 2.0
DeclareParameter "y"
y = 3.0

x ~ normal(0.0, 1.0)
y ~ normal(x, 1.0)

DeclareParameter "p"
p ~ betaD(10.0, 1.0)
VarBounds p 0.0 1.0
p = 0.5
MakeLogger p

DeclareParameter "z"
z ~ mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ])
z = 2.5

DeclareParameter "w"
w ~ mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ])
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
theta ~ exponential(1.0)
theta = 1.0
VarBounds theta 0.0 False
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/input4bensprogram.txt") ~ afsGroup (theta)
MakeLogger theta

DeclareParameter "theta1"
DeclareParameter "theta2"
theta1 ~ exponential(1.0)
theta1 = 1.0
theta2 ~ exponential(1.0)
theta2 = 1.0
DeclareParameter "i"
i ~ bernoulli(0.5)
i = False
DeclareParameter "p2"
p2 ~ betaD(1.0, 1.0)
p2 = 0.5
VarBounds p2 0.0 1.0
VarBounds theta1 0.0 False
VarBounds theta2 0.0 False
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/input4bensprogram2.txt") ~ afsMixture ([theta1,theta2],[p2,1.0-p2])
MakeLogger theta1
MakeLogger theta2
MakeLogger p2
MakeLogger i
