x ~ normal(0.0, 1.0)
y ~ normal(x, 1.0)

p ~ betaD(10.0, 1.0)
VarBounds p 0.0 1.0

z ~ mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ])

w ~ mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ])

[x1,x2] ~ iid (2,normal (1.0, 1.0))
x1 := 1.0
x2 := 2.0

theta ~ exponential(1.0)
VarBounds theta 0.0 False
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afsGroup (theta)

theta1 ~ exponential(1.0)
theta2 ~ exponential(1.0)
i ~ bernoulli(0.5)

p2 ~ betaD(1.0, 1.0)
VarBounds p2 0.0 1.0
VarBounds theta1 0.0 False
VarBounds theta2 0.0 False
((alleleFrequencySpectrum . remove2ndAllele . readPhaseFile) "/home/bredelings/Reports/Kmar/BP.phase1.infile") ~ afsMixture ([theta1,theta2],if i then [p2,1.0-p2] else [1.0,0.0])
