module Probability.Distribution.Tree.Conditioning where

-- Crown denotes the observed crown: both initial lineages have sampled descendants.
data Start = Origin | Crown

data AgeTreatment = AtAge Double | Flat

-- These retain their birth-death meanings even when survival is certain under Yule.
data Condition = Unconditioned | Surviving | Tips Int

-- Validate configuration separately from tree support. The count is observed through the taxa,
-- even when its probability is retained in the density rather than conditioned away.
validateTreeConditioning :: String -> Int -> Start -> AgeTreatment -> Condition -> ()
validateTreeConditioning name n start age condition
    | n < 1 = error $ name ++ ": Origin requires at least one taxon"
    | otherwise = checkStart `seq` checkAge `seq` checkCount
    where
      checkStart = case start of
          Crown | n < 2 -> error $ name ++ ": Crown requires at least two taxa"
          _ -> ()
      checkAge = case (start, age, condition) of
          (Crown, Flat, _) -> error "Flat age treatment with Crown is not implemented yet."
          (Origin, Flat, Tips _) -> ()
          (Origin, Flat, _) -> error $ "Flat age treatment requires tip-count conditioning. "
                                    ++ "Use Tips n, or specify an age with AtAge."
          _ -> ()
      checkCount = case condition of
          Tips k | k /= n -> error $ name ++ ": Tips n must match the number of taxa"
          _ -> ()
