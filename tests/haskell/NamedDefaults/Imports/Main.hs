{-# LANGUAGE NoImplicitPrelude #-}
-- Module-boundary defaults need coverage beyond the local-default tests: imports must
-- resolve all candidates, retain unrelated defaults, and work without NamedDefaults here.
module Main where
import Compiler.Error (error)
import System.IO (print)
import Reexport
import qualified Override
main = print (choose (error "unused"), other (error "unused"), Override.result)
