module Demo1 where
{
import Distributions;

main = do
{
  p <- sample $ beta 5.0 1.0;

  n <- sample $ geometric p;

  return (Nothing,[
           ("p",(Just p,[])),
           ("n",(Just n,[]))
          ]);
}
}
