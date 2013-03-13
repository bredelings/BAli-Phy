module Test where
{
  import PopGen;
  import Distributions;
  get_observed_alleles file = map list_from_vector $ list_from_vector $ read_phase_file file;

  filename = "/home/bredelings/Reports/Kmar/BP.phase1.infile";

  n = 5;
  
note mean ~ iid(n, gamma(0.5,0.5) );
note sigmaOverMu ~ iid(n, gamma(1.05,0.1) );
  
  a = map (\x->1.0/(x^2)) sigmaOverMu;
  b = zipWith (/) mean a;

  alpha = 1.0;
  
note q ~ iid (n, beta(1.0,alpha) );

  normalize l = let {total = sum l} in map (/total) l;

  q' = map (1.0-) q;
  left = [ product (take i q') | i <- take n [0..]];
  p' = zipWith (*) q left;
  
  p = normalize p';
  
  thetaDist = mixture [ (p!!i, gamma (a!!i,b!!i)) | i <- take n [0..]];

note theta_example ~ thetaDist;
  
  data1 = get_observed_alleles filename;
  
  n_loci = length data1;
  
  n_individuals = (length (data1!!0))/2;

note s ~ uniform(0.0, 1.0);

note theta ~ iid (n_loci, thetaDist);

note t ~ iid(n_individuals, exponential (s/(1.0-s)));

note i ~ iid(n_loci, plate (n_individuals,\k->bernoulli (1.0-0.5**t!!k)) );

note data data1 ~ plate (n_loci, \l -> afs2 (theta!!l,i!!l));
note MakeLogger (p!!0);
note MakeLogger (p!!1);
note MakeLogger (p!!2);
note MakeLogger (p!!3);
note MakeLogger (p!!4);
}