module AirLine where {
import Distributions;

fatalities = [24,25,31,31,22,21,26,20,16,22];

indices' i [] = [];
indices' i (x:xs) = i:(indices' (i+1.0) xs);
indices l = indices' 0.0 l;

observe_list ys dists = sequence_ [Observe y dist | (y,dist) <- zip ys dists];

safe_exp x = if (x < (-700.0)) then
                exp(-700.0);
             else if (x > 700.0) then
                exp 700.0;
             else
                exp x;

main = Prefix "Airline" $ do 
{
  alpha <- cauchy 0.0 1.0;
  Log "alpha" alpha;

  beta <- cauchy 0.0 1.0;
  Log "beta" beta;

  observe_list fatalities [poisson $ safe_exp(alpha + beta*i) | i <- indices fatalities]
};

}
