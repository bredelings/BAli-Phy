module Demo2 where
{
import Distributions;

main = do
{
  xs <- iid 10 (normal 0.0 1.0);
  Log "xs" xs;

  let {ys = map (\x -> x*x) xs};
  Log "ys" ys;

  Log "sum" (sum ys);
}
}
