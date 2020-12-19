import           Probability

import           Tree

main = sample $ do
    xs <- crp 2.0 10 2
    return ["xs" %=% xs]

