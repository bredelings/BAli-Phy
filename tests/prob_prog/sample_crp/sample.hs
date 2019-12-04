import           Probability

import           Tree

main = random $ do
    xs <- crp 2.0 10 2
    return ["xs" %=% xs]

