module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/input-short.txt";
  
  note mean1 ~ exponential(1.0);
  note sigmaOverMu1 ~ exponential(0.1);
  
  note mean2 ~ exponential(1.0);
  note sigmaOverMu2 ~ exponential(0.1);
  
  a1 = 1.0/(sigmaOverMu1^2);
  b1 = mean1/a1;
  
  a2 = 1.0/(sigmaOverMu2^2);
  b2 = mean2/a2;

  note p ~ beta(2.0, 2.0);
  
  thetaDist = mixture [(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))];
  note theta1 ~ thetaDist;
  note theta2 ~ thetaDist;
  note theta3 ~ thetaDist;
  note theta4 ~ thetaDist;
  thetas = [theta1,theta2,theta3,theta4];

  d1 = getAFS filename;
  note data d1 ~ plate (length d1, \i -> afs (thetas!!i))
}