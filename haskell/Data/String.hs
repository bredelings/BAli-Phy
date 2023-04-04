module Data.String where

class IsString a where
    fromString :: String -> a

instance a ~ Char => IsString [a] where
    fromString x = x

type String = [Char]

-- lines :: String -> [String]
-- words :: String -> [String]
-- unlines :: [String] -> String
-- unwords :: [String] -> String
