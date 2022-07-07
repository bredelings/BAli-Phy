module Test where

xys = [(1,2),(3,4)] :: [(Int,Int)]
xxs = [x*x | (x,y) <- xys]
(x,y) = head xys
