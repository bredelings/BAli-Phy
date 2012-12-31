module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/input-short.txt";
  
  a1 = 1.0/(sigmaOverMu1^2);
  b1 = mean1/a1;
  
  a2 = 1.0/(sigmaOverMu2^2);
  b2 = mean2/a1;
  
  thetas = [theta1,theta2,theta3,theta4];
  d1 = getAFS filename;
}  
{
  mean1 ~ exponential(1.0);
  sigmaOverMu1 ~ exponential(0.1);
  
  mean2 ~ exponential(1.0);
  sigmaOverMu2 ~ exponential(0.1);
  
  p ~ beta(2.0, 2.0);
  
  theta1 ~ mixture ([(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))]);
  theta2 ~ mixture ([(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))]);
  theta3 ~ mixture ([(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))]);
  theta4 ~ mixture ([(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))]);

  data d1 ~ plate (length d1, \i -> afs thetas!!i)
}