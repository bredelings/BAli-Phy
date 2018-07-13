module Demo4 where
{
import Distributions;

main = do
{
  xs <- sample $ iid 10 (normal 0.0 1.0);

  categories <- sample $ iid 10 (categorical (replicate 10 0.1));

  let {ys = [xs!!(categories!!i) | i <- [0..9]]};
  return (Nothing,[("ys",(Just ys,[]))]);
}
}
