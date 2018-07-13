module Demo5 where
{
import Distributions;

random_walk x0 0 _ = return [x0];
random_walk x0 n f = do {
                           x1 <- sample $ f x0;
                           xs <- random_walk x1 (n-1) f;
                           return (x0:xs);
                        };

main = do
{
  zs <- random_walk 0.0 10 (\mu -> normal mu 1.0);

  Observe 2.0 (normal (zs!!10) 0.1);
  return (Nothing,[("zs",(Just zs,[]))]);
}
}
