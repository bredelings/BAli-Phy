import           Probability

model = sample $ do
    xs <- crp 2.0 10 2
    return ["xs" %=% xs]

main = do
  mcmc model
