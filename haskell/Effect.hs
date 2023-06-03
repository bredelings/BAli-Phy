module Effect where

-- An Effect may be a node in a graph??
data Effect

foreign import bpcall "Modifiables:" getProperties :: a -> b
