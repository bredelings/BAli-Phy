import Probability
import Data.Frame

-- Shift the value when mu or sigma changes.
normal' mu sigma = do
  z <- sample $ normal 0 1
  return $ mu + z*sigma

model floor_values county_code_values log_radon_data = do

  let n_counties = length $ nub $ county_code_values

  mu_a <- sample $ normal 0 (100**2)
  sigma_a <- sample $ half_cauchy 5

  mu_b <- sample $ normal 0 (100**2)
  sigma_b <- sample $ half_cauchy 5

  -- This basically associates 0..(n-1) with normal a/b
  a <- sample $ iid n_counties (normal' mu_a sigma_a)
  b <- sample $ iid n_counties (normal' mu_b sigma_b)

  -- This constructs the distribution of predicted values given county_code and floor
  eps <- sample $ half_cauchy 5
  let dist county_code floor = normal (a!!county_code + b!!county_code*floor) eps

  let loggers = ["mu_a" %=% mu_a, "sigma_a" %=% sigma_a, "mu_b" %=% mu_b, "sigma_b" %=% sigma_b]

  observe log_radon_data $ independent [ dist county_code floor | (floor,county_code) <- zip floor_values county_code_values]

  return loggers

main = do

  radon <- readTable "radon.csv"

  let floor_values       = radon $$ "floor"       :: [Double]
      county_code_values = radon $$ "county_code" :: [Int]
      log_radon_data     = radon $$ "log_radon"   :: [Double]

  mcmc $ model floor_values county_code_values log_radon_data

