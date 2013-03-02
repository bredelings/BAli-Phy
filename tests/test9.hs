module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

note mean ~ iid(5, gamma(0.5,0.5) );
note sigmaOverMu ~ iid(5, gamma(1.05,0.1) );
  
  a1 = 1.0/(sigmaOverMu!!0^2);
  b1 = mean!!0/a1;
  
  a2 = 1.0/(sigmaOverMu!!1^2);
  b2 = mean!!1/a2;

  a3 = 1.0/(sigmaOverMu!!2^2);
  b3 = mean!!2/a3;

  a4 = 1.0/(sigmaOverMu!!3^2);
  b4 = mean!!3/a4;

  a5 = 1.0/(sigmaOverMu!!4^2);
  b5 = mean!!4/a5;

  alpha = 1.0;
  
note q ~ iid (5, beta(1.0,alpha) );

  p1' = q!!0;
  p2' = (1.0-q!!0)*q!!1;
  p3' = (1.0-q!!0)*(1.0-q!!1)*q!!1;
  p4' = (1.0-q!!0)*(1.0-q!!1)*(1.0-q!!2)*q!!3;
  p5' = (1.0-q!!0)*(1.0-q!!1)*(1.0-q!!2)*(1.0-q!!3)*q!!4;
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

note theta_example ~ thetaDist;
  
note theta ~ iid (32, thetaDist);

  d1 = getAFS filename;
note data d1 ~ plate (length d1, \i -> afs (theta!!i));
note MakeLogger p1;
note MakeLogger p2;
note MakeLogger p3;
note MakeLogger p4;
note MakeLogger p5;
}