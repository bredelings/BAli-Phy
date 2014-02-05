module TN where
{
import SModel;
import Distributions;

main = Prefix "TN" 
  (do {
     kappaPur <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPur" kappaPur;
     kappaPyr <- logLaplace (log 2.0) 0.25 ;
     Log "kappaPyr" kappaPyr;
     return $ \nuca -> tn nuca kappaPur kappaPyr
});
}
