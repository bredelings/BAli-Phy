module TN where
{
import SModel;
import Distributions;

main nuca = Prefix "TN" 
  (do {
     kappaPur <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPur" kappaPur;
     kappaPur <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPyr" kappaPyr;
     return $ tn nuca kappaPur kappaPyr
});
}
