import           Probability

model = do
    n <- prior $ geometric 0.5
    ys <- prior $ iid n (exponential 1)
    observe 3 $ normal (sum ys) 1
    return ["n" %=% n, "ys" %=% ys]

main = do
  return model
