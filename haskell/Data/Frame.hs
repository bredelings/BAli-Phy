module Data.Frame ( csv_to_frame, readTable, ($$), FDouble, FInt, FString )
    where

import qualified Data.Map as Map
import Data.CSV

-- This gives the csv file as a list of rows
csv_to_frame :: [ [ String ] ] -> Map String [String]

csv_to_frame (header:rows) = Map.fromList [(field_name, map (!!i) rows) | (i,field_name) <- zip [0..] header]

readTable = csv_to_frame . read_csv

data FieldType = FDouble | FInt | FString

-- This does NOT fit the typesystem, but we can fix it when we get the type system
-- ($$) :: Frame -> String -> [a]
-- frame ($$) fname = map read (frame Map.! fname) :: [a]
frame $$ (field,FDouble) = map read_double $ (frame Map.! field)
frame $$ (field,FInt)    = map read_int    $ (frame Map.! field)
frame $$ (field,FString) = (frame Map.! field)
