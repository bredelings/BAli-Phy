module AirLine where {
import Distributions;

fatalities = [24,25,31,31,22,21,26,20,16,22];

iotaZip i [] = [];
iotaZip i (x:xs) = (i,x):(iotaZip (i+1.0) xs);

main = Prefix "Airline" $ do 
{
  alpha <- uniform (-2.0) 2.0;
  Log "alpha" alpha;

  beta <- uniform (-2.0) 2.0;
  Log "beta" beta;

  sequence_ [Observe y (poisson mu) | (i,y) <- iotaZip 0.0 fatalities, let {mu = exp(alpha + beta*i)}];
};
}
