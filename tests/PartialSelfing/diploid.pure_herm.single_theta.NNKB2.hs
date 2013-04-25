module Test where
{
  import PopGen;
  import Distributions;
  get_observed_alleles file = map list_from_vector $ list_from_vector $ read_phase_file file;

  filename = "/home/bredelings/Reports/Kmar/NNKB2.phase1.infile";

  data1 = get_observed_alleles filename;

  n_loci = length data1;

  n_individuals = (length (data1!!0))/2;

note theta ~ gamma(0.5,0.5);

  theta_effective = theta*(2.0-s);

note s ~ uniform(0.0, 1.0);

note t' ~ iid(n_individuals, exponential (-1.0/log s));

  t = map truncate t';

note i ~ iid(n_loci, plate (n_individuals,\k->bernoulli (1.0-0.5**t!!k)) );

note data data1 ~ plate (n_loci, \l -> afs2 (theta_effective,i!!l));

note MakeLogger t;
}