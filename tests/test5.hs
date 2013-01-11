module Test where
{
  getAFS = (alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);
  filename1 = "/home/bredelings/Reports/Kmar/input4bensprogram.txt";
  
  filename2 = "/home/bredelings/Reports/Kmar/input4phase_0.5_2_2000.txt";
  
  note x ~ normal(0.0, 1.0);
  note y ~ normal(x, 1.0);

  note p ~ betaD(10.0, 1.0);
  note VarBounds p 0.0 1.0;

  note z ~ mixture([( p, normal(x,1.0) ),(1.0-p, normal(y,1.0)) ]);

  note w ~ mixture([( 0.5, normal(-2.0,1.0) ),(0.5, normal(2.0,1.0)) ]);

  note [x1,x2] ~ iid (2,normal (1.0, 1.0));
  note x1 := 1.0;
  note x2 := 2.0;

  note theta ~ exponential(1.0);
  note VarBounds theta 0.0 False;
  note data getAFS filename1 ~ afsGroup (theta);

  note theta1 ~ exponential(1.0);
  note theta2 ~ exponential(1.0);

  note i ~ bernoulli(0.5);

  note p2 ~ betaD(2.0, 2.0);
  note VarBounds p2 0.0 1.0;
  note VarBounds theta1 0.0 False;
  note VarBounds theta2 0.0 False;
  note data getAFS filename2 ~ afsMixture ([theta1,theta2],[p2,1.0-p2]);
}