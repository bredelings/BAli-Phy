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
MakeLogger "BUGS.p"

DeclareParameter "z"
z ~ Mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ])
z = 2.5

DeclareParameter "w"
w ~ Mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ])
w = 0.0
MakeLogger "BUGS.w"

DeclareParameter "x1"
DeclareParameter "x2"
[x1,x2] ~ iid (2,normal (1.0, 1.0))
x1 = 1.0
x2 = 2.0
MakeLogger "BUGS.x1"
MakeLogger "BUGS.x2"

DeclareParameter "theta"
theta ~ Exponential(1.0)
theta = 1.0
VarBounds theta 0.0 false
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afs (theta)
MakeLogger "BUGS.theta"

DeclareParameter "theta1"
theta1 ~ Exponential(1.0)
theta1 = 1.0
VarBounds theta1 0.0 false

DeclareParameter "theta2"
theta2 ~ Exponential(1.0)
theta2 = 1.0
VarBounds theta2 0.0 false

DeclareParameter "theta3"
theta3 ~ Exponential(1.0)
theta3 = 1.0
VarBounds theta3 0.0 false

DeclareParameter "p1"
p1 = 0.33
VarBounds p1 0.0 1.0

DeclareParameter "p2"
p2 = 0.33
VarBounds p2 0.0 1.0

DeclareParameter "p3"
p3 = 1.0 - p1 - p2
VarBounds p3 0.0 1.0

[p1,p2,p3] ~ dirichlet([10.0, 10.0, 10.0])

DeclareParameter "i"
i ~ bernoulli (0.5)
i = false



((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afsMixture (if i then [theta1,theta2,theta3] else [theta1,theta2],if i then [p1,p2,p3] else [p1/(p1+p2),p2/(p1+p2)])
MakeLogger "BUGS.theta1"
MakeLogger "BUGS.theta2"
MakeLogger "BUGS.theta3"
MakeLogger "BUGS.p1"
MakeLogger "BUGS.p2"
MakeLogger "BUGS.p3"
MakeLogger "BUGS.i"
