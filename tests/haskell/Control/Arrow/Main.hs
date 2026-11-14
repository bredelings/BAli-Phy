{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Control.Arrow
import Control.Applicative
import Control.Monad
import qualified Control.Category as C
import Compiler.Error (error)
import Compiler.Num
import Data.Function (($), const)
import Data.Tuple (fst, snd)
import Data.Either
import Data.Maybe
import Data.Functor
import Data.List (map)
import System.IO (print)

-- Check the standard Arrow instances and lazy tuple/feedback behavior, which trie tests alone
-- would not exercise. This remains useful while these instances are maintained locally.
main = do
 print (first (+1) (2::Int,3::Int), second (+1) (2::Int,3::Int))
 print (((+1) *** (*2)) (2::Int,3::Int), ((+1) &&& (*2)) (3::Int))
 print (((+1) C.. (*2)) (3::Int), ((*2) >>> (+1)) (3::Int))
 print (map ((+1) +++ (*2)) [Left (2::Int),Right (3::Int)])
 print (map ((+1) ||| (*2)) [Left (2::Int),Right (3::Int)])
 print (fst (first (const (7::Int)) (error "tuple forced")))
 print (snd (second (const (8::Int)) (error "tuple forced")))
 let f = Kleisli (\x -> Just (x+1::Int))
     g = Kleisli (\x -> Just (2*x::Int))
 print (runKleisli (f >>> g) 3, runKleisli (first f) (3,4::Int))
 print (runKleisli (f +++ g) (Right 3), runKleisli (leftApp f) (Left 3::Either Int Int))
 print (runKleisli (app::Kleisli Maybe (Kleisli Maybe Int Int,Int) Int) (f,3))
 print (runKleisli (f <+> zeroArrow) 3)
 print (runKleisli (fmap (+2) f) 3, runKleisli (pure (+) <*> f <*> g) 3)
 print (runKleisli (f >>= \x -> pure (x+5)) 3)
 let ArrowMonad result = (ArrowMonad (const (3::Int)) >>= \x -> pure (x+2))
 print (result ())
 print (loop (\(x,d) -> (d,x+1)) (3::Int))
 print (runKleisli (loop (Kleisli (\(x,d) -> Just (d,x+1)))) (3::Int))
