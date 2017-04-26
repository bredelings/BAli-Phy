module Demo1 where
{
import Distributions;

main = do
{
  p <- beta 10.0 1.0;
  Log "p" p;

  n <- geometric p;
  Log "n" n;
}
}
