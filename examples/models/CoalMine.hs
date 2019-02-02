module CoalMine where
import Distributions

fatalities = [4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 3, 3, 0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1]
years = [1851 .. 1962]

indices' i [] = []
indices' i (x:xs) = i:(indices' (i+1.0) xs)
indices l = indices' 0.0 l

observe_list ys dists = sequence_ [observe dist y | (y,dist) <- zip ys dists]

main = do

  eta <- sample $ gamma 10.0 20.0

  lambda <- sample $ gamma 2.0 eta

  gamm <- sample $ gamma 2.0 eta

  theta <- sample $ uniform 1852.0 1962.0

  let mean year = if (theta > intToDouble year) then lambda else gamm

  observe_list fatalities [poisson (mean year) | year <- years]
  return $ log_all [eta %% "eta",
                    lambda %% "lambda",
                    gamma %% "gamma",
                    theta %% "theta"]
