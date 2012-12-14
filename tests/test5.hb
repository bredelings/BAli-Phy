module Test where
{
  getAFS = (alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);
  filename1 = "/home/bredelings/Reports/Kmar/input4bensprogram.txt";
  
  filename2 = "/home/bredelings/Reports/Kmar/input4phase_0.5_2_2000.txt"
}  
{
  x ~ normal(0.0, 1.0);
  y ~ normal(x, 1.0);

  p ~ betaD(10.0, 1.0);

  z ~ mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ]);

  w ~ mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ]);

  [x1,x2] ~ iid (2,normal (1.0, 1.0));
  x1 := 1.0;
  x2 := 2.0;

  theta ~ exponential(1.0);
  data getAFS filename1 ~ afsGroup (theta);

  theta1 ~ exponential(1.0);
  theta2 ~ exponential(1.0);

  i ~ bernoulli(0.5);

  p2 ~ betaD(2.0, 2.0);
  VarBounds theta1 (above 0.0);
  data getAFS filename2 ~ afsMixture ([theta1,theta2],[p2,1.0-p2])
}