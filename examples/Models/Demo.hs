module Demo where
{
import Distributions;

-- Start from x0, sample n additional points.
-- (f x) is the distribution of the point after x.
random_walk x0 0 _ = return [x0];
random_walk x0 n f = do {
                           x1 <- f x0;
                           xs <- random_walk x1 (n-1) f;
                           return (x0:xs);
                        };

main = do
{
  p <- beta 10.0 1.0;
  Log "p" p;

  n <- geometric p;
  Log "n" n;

  q <- cauchy 0.0 1.0;
  Log "q" q;

  x <- iid 10 (normal 0.0 1.0);
  Log "x" x;
 
  y <- list [normal (x!!i) 1.0 | i <- [0..9]];
  Log "y" y;

  z <- random_walk 0.0 10 (\mu -> normal mu 1.0);
  Log "z" z;

  Observe 2.0 (normal (z!!10) 1.0);
}
}
