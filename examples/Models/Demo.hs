module Demo where
{
import Distributions;

main = do
{
  n <- geometric 0.5;
  Log "n" n;

  p <- beta 10.0 1.0;
  Log "p" p;

  q <- cauchy 0.0 1.0;
  Log "q" q;

  x <- iid 10 (normal 0.0 1.0);
  Log "x" x;
 
  y <- list [normal (x!!i) 1.0 | i <- [0..9]];
  Log "y" y;
}
}
