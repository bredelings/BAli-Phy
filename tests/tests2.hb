DeclareParameter "x"
x = 2.0
DeclareParameter "y"
y = 3.0

x ~ Normal(0.0, 1.0)
y ~ Normal(x, 1.0)

DeclareParameter "p"
p ~ Beta(10.0, 1.0)
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
theta ~ Exponential(0.1)
theta = 1.0
((alleleFrequencySpectrum . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afs (theta)
MakeLogger "BUGS.theta"