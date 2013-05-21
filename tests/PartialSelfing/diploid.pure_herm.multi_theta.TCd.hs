module Test where
{
  import PopGen;
  import Distributions;
  get_observed_alleles file = map list_from_vector $ list_from_vector $ read_phase_file file;

  filename = "/home/bredelings/Reports/Kmar/TwinCays2005a.phase1.infile";

  n = 5;

note mean ~ iid(n, gamma(0.5,0.5) );
note sigmaOverMu ~ iid(n, gamma(1.05,0.1) );

  alpha = 1.0;

note p ~ dirichlet' (n, alpha/(intToDouble n));

  data1 = get_observed_alleles filename;

  n_loci = length data1;

  n_individuals = (length (data1!!0))/2;

note category ~ iid(n_loci, categorical p);

note z ~ iid(n_loci, normal(0.0, 1.0));

  safe_exp x = if (x < (-20.0)) then
                 exp (-20.0);
               else if (x > 20.0) then
                 exp 20.0;
               else
                 exp x;

  theta = [ mean!!k * safe_exp (z!!i * sigmaOverMu!!k) | i <- take n_loci [0..], let {k=category!!i}];

  theta_effective = map (*(2.0-s)) theta;

note theta_example ~ mixture [ (p!!i, logNormal(log(mean!!i),sigmaOverMu!!i)) | i <- take n [0..] ];

note s ~ uniform(0.0, 1.0);

note t' ~ iid(n_individuals, exponential (-1.0/log s));

  t = map truncate t';

note i ~ plate(n_individuals, \k->iid(n_loci, bernoulli (1.0-0.5**t!!k)) );

note data data1 ~ plate (n_loci, \l -> afs2 (theta_effective!!l,map (!!l) i));

note MakeLogger p;
note MakeLogger theta;
note MakeLogger t;
note MakeMove (sum_out_coals (t'!!0) (i!!0));
}