module Test where
{
  import Distributions;
  note i ~ bernoulli 0.5;
  note [x1,x2] ~ iid (2,exponential 0.5);
  note y ~ iid (2, iid(3, exponential 0.5) );
  note z ~ exponential ((y!!0)!!0)
}
