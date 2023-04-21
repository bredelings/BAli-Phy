import           Probability

model = do
    xs <- sample $ crp 2 10 2
    return ["xs" %=% xs]

main = mcmc model
