module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

note mean ~ iid(5, gamma(0.5,0.5) );
note sigmaOverMu ~ iid(5, gamma(1.05,0.1) );
  
  a = map (\x->1.0/(x^2)) sigmaOverMu;
  b = map (\(a,m)->m/a) (zip a mean);

  alpha = 1.0;
  
  prefixes (x:xs) = [x]:map (\l->x:l) (prefixes xs);

note q ~ iid (5, beta(1.0,alpha) );

  normalize l = let {total = sum l} in map (\x-> x/total) l;

  q' = map (\x->1.0-x) q;
  left = map (\i->product (take i q')) [0..4];
  p' = zipWith (*) q left;
  
  p = normalize p';
  
  
  thetaDist = mixture [(p!!0, gamma (a!!0,b!!0)), 
                       (p!!1, gamma (a!!1,b!!1)),
                       (p!!2, gamma (a!!2,b!!2)),
                       (p!!3, gamma (a!!3,b!!3)),
                       (p!!4, gamma (a!!4,b!!4))
                      ];

note theta_example ~ thetaDist;
  
note theta ~ iid (length d1, thetaDist);

  d1 = getAFS filename;
note data d1 ~ plate (length d1, \i -> afs (theta!!i));
note MakeLogger (p!!0);
note MakeLogger (p!!1);
note MakeLogger (p!!2);
note MakeLogger (p!!3);
note MakeLogger (p!!4);
}