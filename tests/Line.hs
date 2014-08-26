module Line where
{
import Data.ReadFile;
import Distributions;
import System.Environment;

xs = read_file_as_double "xs";

ys = read_file_as_double "ys";

main = Prefix "Line" $ do 
{
  b <- normal 0.0 1.0;
  Log "b" b;

  m <- normal 0.0 1.0;
  Log "m" m;

  s <- gamma 1.0 1.0;
  Log "s" s;

  let {y_hat x = m*x + b};

  sequence_ [Observe y (normal (y_hat x) s) | (x,y) <- zip xs ys];
};

}
