module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

note mean1 ~ gamma(0.5,0.5);
note sigmaOverMu1 ~ gamma(1.05,0.1);
  
note mean2 ~ gamma(0.5,0.5);
note sigmaOverMu2 ~ gamma(1.05,0.1);
  
note mean3 ~ gamma(0.5,0.5);
note sigmaOverMu3 ~ gamma(1.05,0.1);
  
note mean4 ~ gamma(0.5,0.5);
note sigmaOverMu4 ~ gamma(1.05,0.1);
  
note mean5 ~ gamma(0.5,0.5);
note sigmaOverMu5 ~ gamma(1.05,0.1);
  
  a1 = 1.0/(sigmaOverMu1^2);
  b1 = mean1/a1;
  
  a2 = 1.0/(sigmaOverMu2^2);
  b2 = mean2/a2;

  a3 = 1.0/(sigmaOverMu3^2);
  b3 = mean3/a3;

  a4 = 1.0/(sigmaOverMu4^2);
  b4 = mean4/a4;

  a5 = 1.0/(sigmaOverMu5^2);
  b5 = mean5/a5;

  alpha = 1.0;
  
note [q1,q2,q3,q4,q5] ~ iid (5, beta(1.0,alpha) );

  p1' = q1;
  p2' = (1.0-q1)*q2;
  p3' = (1.0-q1)*(1.0-q2)*q3;
  p4' = (1.0-q1)*(1.0-q2)*(1.0-q3)*q4;
  p5' = (1.0-q1)*(1.0-q2)*(1.0-q3)*(1.0-q4)*q5;
  sum = p1' + p2' + p3' + p4' + p5';
  p1 = p1'/sum;
  p2 = p2'/sum;
  p3 = p3'/sum;
  p4 = p4'/sum;
  p5 = p5'/sum;
  
  
  thetaDist = mixture [(p1, gamma (a1,b1)), 
                       (p2, gamma (a2,b2)),
                       (p3, gamma (a3,b3)),
                       (p4, gamma (a4,b4)),
                       (p5, gamma (a5,b5))
                      ];

note theta33 ~ thetaDist;
  thetas = [theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10,theta11,theta12,theta13,theta14,theta15,theta16,theta17,theta18,theta19,theta20,theta21,theta22,theta23,theta24,theta25,theta26,theta27,theta28,theta29,theta30,theta31,theta32];
  
note [theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10,theta11,theta12,theta13,theta14,theta15,theta16,theta17,theta18,theta19,theta20,theta21,theta22,theta23,theta24,theta25,theta26,theta27,theta28,theta29,theta30,theta31,theta32] ~ iid (32, thetaDist);

  d1 = getAFS filename;
note data d1 ~ plate (length d1, \i -> afs (thetas!!i));
note MakeLogger p1;
note MakeLogger p2;
note MakeLogger p3;
note MakeLogger p4;
note MakeLogger p5;
}