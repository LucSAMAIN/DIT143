import Prelude hiding (sin, cos) -- handle name issue with our functions


-- A, see videos: https://play.chalmers.se/playlist/dedicated/0_bmaetfz1/0_n6gvtmek (lecture 4A)
data Expr
  = Num Double         -- Use Double for numbers as per the question
  | Add Expr Expr      -- Addition
  | Mul Expr Expr      -- Multiplication
  | Sin Expr           -- Sinus
  | Cos Expr           -- Cosinus
  | X                  -- Variable "x"
  deriving (Eq, Show)

-- Simple functions to construct exprs
x :: Expr
x = X

num :: Double -> Expr
num dValue = Num dValue

add :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2

mul :: Expr -> Expr -> Expr
mul e1 e2 = Mul e1 e2

sin :: Expr -> Expr
sin e = Sin e

cos :: Expr -> Expr
cos e = Cos e

size :: Expr -> Integer
size (Num value) = 0
size (X) = 0
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2



exampleExpr :: Expr
exampleExpr = cos (sin (x `add` (num 3.0)))