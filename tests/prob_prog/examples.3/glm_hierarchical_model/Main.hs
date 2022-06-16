import Probability
import Data.Frame

-- Shift the value when mu or sigma changes.
normal' mu sigma = do
  z <- normal 0.0 1.0
  return $ mu + z*sigma

model floor_values county_code_values log_radon_data = do

  let n_counties = length $ nub $ county_code_values

  mu_a <- normal 0.0 (100.0**2.0)
  sigma_a <- half_cauchy 0.0 5.0

  mu_b <- normal 0.0 (100.0**2.0)
  sigma_b <- half_cauchy 0.0 5.0

  -- This basically associates 0..(n-1) with normal a/b
  a <- iid n_counties (normal' mu_a sigma_a)
  b <- iid n_counties (normal' mu_b sigma_b)

  -- This constructs the distribution of predicted values given county_code and floor
  eps <- half_cauchy 0.0 5.0
  let dist county_code floor = normal (a!!county_code + b!!county_code*floor) eps

  let loggers = ["mu_a" %=% mu_a, "sigma_a" %=% sigma_a, "mu_b" %=% mu_b, "sigma_b" %=% sigma_b]

  log_radon_data ~> independent [ dist county_code floor | (floor,county_code) <- zip floor_values county_code_values]

  return loggers

main = do

  radon <- readTable "radon.csv"

  let floor_values       = radon $$ "floor",      :: [Double]
      county_code_values = radon $$ "county_code" :: [Int]
      log_radon_data     = radon $$ "log_radon"   :: [Double]

  mcmc $ model floor_values county_code_values log_radon_data

