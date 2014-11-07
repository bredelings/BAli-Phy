module Demo4 where
{
import Distributions;

main = do
{
  xs <- iid 10 (normal 0.0 1.0);

  categories <- iid 10 (categorical (replicate 10 0.1));

  let {ys = [xs!!(categories!!i) | i <- [0..9]]};
  Log "ys" ys;
}
}
