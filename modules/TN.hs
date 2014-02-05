module TN where
{
  import SModel;
  import Distributions;
  note kappaPur ~ logLaplace (log 2.0) 0.25 ;
  note kappaPyr ~ logLaplace (log 2.0) 0.25 ;
  main = return $ (\nuca -> tn nuca kappaPur kappaPyr)
}
