module LiStephens2003 where

import           Probability         -- for the model framework

import           System.Environment  -- for getArgs
import           Data.Frame          -- for readTable & friends
import           Bio.Alignment       -- for load_alignment
import           Bio.Alphabet        -- for dna
import           PopGen              -- for li_stephens_2003

model locs sequence_data = do

  rho <- sample $ logLaplace 0.01 2

  let len = 1 + fromIntegral (last locs)

  observe sequence_data $ li_stephens_2003 locs [(rho,0,len)]

  return ["rho" %=% rho ]

main logDir = do

  (seq_filename:locs_filename:_) <- getArgs

  sequence_data <- load_alignment dna seq_filename

  locs_table <- readTable locs_filename
  let locs = locs_table $$ "locs" :: [Int]

  return $ model locs sequence_data
