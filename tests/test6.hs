module Test where
{
  import PopGen;
  import Distributions;
  getAFS = (listFromVectorVectorInt . alleleFrequencySpectrum . remove2ndAllele . readPhaseFile);

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

  note mean1 ~ exponential(0.25);
  note sigmaOverMu1 ~ exponential(0.2);
  
  note mean2 ~ exponential(0.25);
  note sigmaOverMu2 ~ exponential(0.2);
  
  a1 = 1.0/(sigmaOverMu1^2);
  b1 = mean1/a1;
  
  a2 = 1.0/(sigmaOverMu2^2);
  b2 = mean2/a2;

  note p ~ beta(4.0, 4.0);
  
  thetaDist = mixture [(p,gamma (a1,b1)), (1.0-p,gamma (a2,b2))];
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
note theta34 ~ thetaDist;
note theta35 ~ thetaDist;
note theta36 ~ thetaDist;
note theta37 ~ thetaDist;
note theta38 ~ thetaDist;
note theta39 ~ thetaDist;
note theta40 ~ thetaDist;
note theta41 ~ thetaDist;
note theta42 ~ thetaDist;
note theta43 ~ thetaDist;
note theta44 ~ thetaDist;
note theta45 ~ thetaDist;
note theta46 ~ thetaDist;
note theta47 ~ thetaDist;
note theta48 ~ thetaDist;
note theta49 ~ thetaDist;
note theta50 ~ thetaDist;
note theta51 ~ thetaDist;
note theta52 ~ thetaDist;
note theta53 ~ thetaDist;
note theta54 ~ thetaDist;
note theta55 ~ thetaDist;
note theta56 ~ thetaDist;
note theta57 ~ thetaDist;
note theta58 ~ thetaDist;
note theta59 ~ thetaDist;
note theta60 ~ thetaDist;
note theta61 ~ thetaDist;
note theta62 ~ thetaDist;
note theta63 ~ thetaDist;
note theta64 ~ thetaDist;
note theta65 ~ thetaDist;
note theta66 ~ thetaDist;
note theta67 ~ thetaDist;
note theta68 ~ thetaDist;
note theta69 ~ thetaDist;
note theta70 ~ thetaDist;
  thetas = [theta1,theta2,theta3,theta4,theta5,theta6,theta7,theta8,theta9,theta10,theta11,theta12,theta13,theta14,theta15,theta16,theta17,theta18,theta19,theta20,theta21,theta22,theta23,theta24,theta25,theta26,theta27,theta28,theta29,theta30,theta31,theta32,theta33,theta34,theta35,theta36,theta37,theta38,theta39,theta40,theta41,theta42,theta43,theta44,theta45,theta46,theta47,theta48,theta49,theta50,theta51,theta52,theta53,theta54,theta55,theta56,theta57,theta58,theta59,theta60,theta61,theta62,theta63,theta64,theta65,theta66,theta67,theta68,theta69,theta70];

  d1 = getAFS filename;
  note data d1 ~ plate (length d1, \i -> afs (thetas!!i))
}