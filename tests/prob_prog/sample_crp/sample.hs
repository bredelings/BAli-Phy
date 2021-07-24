import           Probability

model = do
    xs <- crp 2.0 10 2
    return ["xs" %=% xs]

main = mcmc model
