module AirLine where 
import Probability

fatalities = [24,25,31,31,22,21,26,20,16,22]

main = do

  alpha <- random $ cauchy 0.0 1.0

  beta <- random $ cauchy 0.0 1.0

  observe (independent [poisson $ safe_exp(alpha + beta*(intToDouble i)) | i <- [0..length fatalities-1]]) fatalities
  return $ log_all [ "alpha" %=% alpha, "beta" %=% beta]
