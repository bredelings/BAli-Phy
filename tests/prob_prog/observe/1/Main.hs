import Probability.Random
import Probability.Distribution.Normal

main = do
  observe (normal 0.0 1.0) 1.0
  return $ log_all []
