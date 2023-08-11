import System.FilePath
import Probability
import Probability.Logger
import MCMC
import qualified Data.Text.IO as T

import qualified Model as Model

main = do
  logParams <- jsonLogger $ "/home/bredelings/Devel/bali-phy/git/tests/prob_prog/demos/2/C1.log.json"

  model <- Model.main

  mymodel <- makeMCMCModel $ do { j <- model; addLogger $ logParams j ; return j }

  jline <- logJSONLine mymodel 0

  T.putStrLn jline
