module Data.Frame ( csv_to_frame, readTable, ($$) )
    where

import qualified Data.Map as Map
import Data.CSV

-- This gives the csv file as a list of rows
csv_to_frame :: [ [ String ] ] -> Map.Map String [String]

csv_to_frame (header:rows) = Map.fromList [(field_name, map (!!i) rows) | (i,field_name) <- zip [0..] header]

readTable filename = csv_to_frame `liftM` read_csv filename

frame $$ field = map read (frame Map.! field)

