module HKY where
{
  import SModel;
  kappa ~ logLaplace( log 2.0, 0.25 );
  main = \nuca -> hky nuca kappa
}