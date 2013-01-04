module M0 where
{
  import SModel;
  import submodel HKY;;
  omega ~ logLaplace(0.0, 0.25);
  main = \codona -> m0 codona (HKY.main codona) omega;
}