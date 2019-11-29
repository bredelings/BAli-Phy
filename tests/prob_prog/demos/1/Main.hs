import Probability

main = random $ do

  p <- beta 5.0 1.0

  n <- geometric p

  return $ log_all [ "p" %=% p, "n" %=% n]
