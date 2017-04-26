module AirLine where {
import Distributions;

fatalities = [4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 3, 3, 0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1];
years = [1851 .. 1962];

indices' i [] = [];
indices' i (x:xs) = i:(indices' (i+1.0) xs);
indices l = indices' 0.0 l;

observe_list ys dists = sequence_ [Observe y dist | (y,dist) <- zip ys dists];

main = Prefix "CoalMine" $ do 
{
  eta <- gamma 10.0 20.0;
  Log "eta" eta;

  lambda <- gamma 2.0 eta;
  Log "lambda" lambda;

  gamm <- gamma 2.0 eta;
  Log "gamma" gamm;

  theta <- uniform 1852.0 1962.0;
  Log "theta" theta;

  let {mean year = if (theta > intToDouble year) then lambda else gamm};

  observe_list fatalities [poisson (mean year) | year <- years];
};

}
