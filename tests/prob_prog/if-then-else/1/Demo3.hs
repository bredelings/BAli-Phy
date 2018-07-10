module Demo3 where
{
import Distributions;

main = do
{
  i <- sample $ bernoulli 0.5;
  y <- sample $ normal 0.0 1.0;
  z <- sample $ exponential 0.1;
  let {x = if (i==1) then y else z};
-- How are we going to log things from stochastic procedures?
-- One way is to return a pair of (value, loggers).
  return $ (Nothing,[("x",(Just x,[]))]);
}
}
