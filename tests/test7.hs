module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

  note mean1 ~ gamma(0.5,0.5);
  note sigmaOverMu1 ~ gamma(0.5,0.5);
  
  note mean2 ~ gamma(0.5,0.5);
  note sigmaOverMu2 ~ gamma(0.5,0.5);
  
  a1 = 1.0/(sigmaOverMu1^2);
  b1 = mean1/a1;
  
  a2 = 1.0/(sigmaOverMu2^2);
  b2 = mean2/a2;

  note p ~ beta(4.0, 4.0);
  
  thetaDist = mixture [(p,logNormal (log mean1,sigmaOverMu1)), (1.0-p,logNormal (log mean2,sigmaOverMu2))];
note theta1 ~ thetaDist;
note theta2 ~ thetaDist;
note theta3 ~ thetaDist;
note theta4 ~ thetaDist;
note theta5 ~ thetaDist;
note theta6 ~ thetaDist;
note theta7 ~ thetaDist;
note theta8 ~ thetaDist;
note theta9 ~ thetaDist;
note theta10 ~ thetaDist;
note theta11 ~ thetaDist;
note theta12 ~ thetaDist;
note theta13 ~ thetaDist;
note theta14 ~ thetaDist;
note theta15 ~ thetaDist;
note theta16 ~ thetaDist;
note theta17 ~ thetaDist;
note theta18 ~ thetaDist;
note theta19 ~ thetaDist;
note theta20 ~ thetaDist;
note theta21 ~ thetaDist;
note theta22 ~ thetaDist;
note theta23 ~ thetaDist;
note theta24 ~ thetaDist;
note theta25 ~ thetaDist;
note theta26 ~ thetaDist;
note theta27 ~ thetaDist;
note theta28 ~ thetaDist;
note theta29 ~ thetaDist;
note theta30 ~ thetaDist;
note theta31 ~ thetaDist;
note theta32 ~ thetaDist;
note theta33 ~ thetaDist;
  thetas = [theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10,theta11,theta12,theta13,theta14,theta15,theta16,theta17,theta18,theta19,theta20,theta21,theta22,theta23,theta24,theta25,theta26,theta27,theta28,theta29,theta30,theta31,theta32];

  d1 = getAFS filename;
  note data d1 ~ plate (length d1, \i -> afs (thetas!!i));
  note MakeLogger (if mean1 < mean2 then p else 1.0-p);
  note MakeLogger (if mean1 < mean2 then mean1 else mean2);
  note MakeLogger (if mean1 > mean2 then mean1 else mean2);
  note MakeLogger (if mean1 < mean2 then sigmaOverMu1 else sigmaOverMu2);
  note MakeLogger (if mean1 > mean2 then sigmaOverMu1 else sigmaOverMu2);
}