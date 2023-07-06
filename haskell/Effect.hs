module Effect where

-- An Effect may be a node in a graph??
data Effect

foreign import bpcall "Modifiables:" getProperties :: a -> b

{-
 For effects, such as registering a prior, we need to be able to
 avoid them when merely computing quantities of interest.

 For example, when computing conditional likelihoods, we don't want
 to force the alignment prior, which can happen since the CLs access
 pairwise alignments inside the AlignmentOnTree object.
 -}
foreign import bpcall "Modifiables:" withEffect :: a -> b -> b
