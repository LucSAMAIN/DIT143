import Test.QuickCheck

data Expr = Num Integer 
          | Add Expr Expr
          | Mul Expr Expr
        deriving Eq

instance Show Expr where
    show = showExpr

-- 2*(3+4) = 14
ex1 = Mul (Num 2) (Add (Num 3) (Num 4))

-- 1 + 2*4 = 9
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

-- (5+1)*3 = 18
ex3 = Mul (Add (Num 5) (Num 1)) (Num 3)

-- (6+2)*(2+1) = 24
ex4 = Mul (Add (Num 6) (Num 2)) (Add (Num 2) (Num 1))

eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

showExpr :: Expr -> String -- reminder
showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) =
    (if isAdd e1 then "(" ++ showExpr e1 ++ ")" else showExpr e1) ++
    " * " ++
    (if isAdd e2 then "(" ++ showExpr e2 ++ ")" else showExpr e2)
  where
    isAdd (Add _ _) = True
    isAdd _         = False



-- random value and all
range = 4

rExpr :: Int -> Gen(Expr)
rExpr s = frequency [(1, rNum), (s, rBin s)] -- Bin for Mul or Add and Num for Num in the "Expr" type
  where 
    rNum = fmap Num (elements [-range..range])
    rBin s = do
          let s' = (s `div` 2)
           op <- elements [Mul, Add]
           expr1 <- rExpr s'
           expr2 <- rExpr s'
           return $ op expr1 expr2
