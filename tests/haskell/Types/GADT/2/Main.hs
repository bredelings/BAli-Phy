data Expr a where
    EBool :: Bool -> Expr Bool
    EInt :: Int -> Expr Int
    ENot :: Expr Bool -> Expr Bool
    ESum :: Expr Int -> Expr Int -> Expr Int
    ECond :: Expr Bool -> Expr a -> Expr a -> Expr a


run_exp :: Expr a -> a
run_exp (EBool b) = b
run_exp (ENot e) = not (run_exp e)
run_exp (EInt i) = i
run_exp (ESum x y) = (run_exp x) + (run_exp y)
run_exp (ECond c x y) = case run_exp c of
                          True -> run_exp x
                          False -> run_exp y

main = putStrLn $ show $ run_exp (ECond (EBool False) (EInt 1) (ESum (EInt 2) (EInt 3)))
