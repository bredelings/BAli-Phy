module AirLine where 
import Distributions

fatalities = [24,25,31,31,22,21,26,20,16,22]

indices' i [] = []
indices' i (x:xs) = i:(indices' (i+1.0) xs)
indices l = indices' 0.0 l

observe_list ys dists = sequence_ [observe dist y | (y,dist) <- zip ys dists]

main = do

  alpha <- sample $ cauchy 0.0 1.0

  beta <- sample $ cauchy 0.0 1.0

  observe_list fatalities [poisson $ safe_exp(alpha + beta*i) | i <- indices fatalities]
  return $ log_all [ alpha %% "alpha", beta %% "beta"]
