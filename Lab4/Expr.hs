import Parsing
import Data.Maybe (fromJust)
import Test.QuickCheck



-- A, see videos: https://play.chalmers.se/playlist/dedicated/0_bmaetfz1/0_n6gvtmek (lecture 4A)
data Expr
  = Num Double         -- Use Double for numbers as per the question
  | Add Expr Expr      -- Addition
  | Mul Expr Expr      -- Multiplication
  | Sin Expr           -- Sinus
  | Cos Expr           -- Cosinus
  | X                  -- Variable "x"
  deriving (Eq, Show)

-- instance Show Expr where
--   show = showExpr

-- Simple functions to construct exprs
x :: Expr
x = X

num :: Double -> Expr
num dValue = Num dValue

add :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2

mul :: Expr -> Expr -> Expr
mul e1 e2 = Mul e1 e2

sinC :: Expr -> Expr
sinC e = Sin e

cosC :: Expr -> Expr -- C stands for constructor handle name issue with our functions
cosC e = Cos e

size :: Expr -> Integer
size (Num value) = 0
size (X) = 0
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2



-- B
showExpr :: Expr -> String
showExpr (Num value) = show value
showExpr (X) = "x"
showExpr (Sin e) = "sin(" ++ showExpr e ++ ")" 
showExpr (Cos e) = "cos(" ++ showExpr e ++ ")" 
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2 -- to deal with the ()
  where showFactor (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
        showFactor e = showExpr e


-- C
eval :: Expr -> Double -> Double
eval (Num value) _ = value
eval (X) xvalue = xvalue
eval (Sin e) xvalue = sin $ eval e xvalue
eval (Cos e) xvalue = cos $ eval e xvalue
eval (Add e1 e2) xvalue = (eval e1 xvalue) + (eval e2 xvalue)
eval (Mul e1 e2) xvalue = (eval e1 xvalue) * (eval e2 xvalue)



-- D see videos from lecture 5A https://play.chalmers.se/playlist/dedicated/0_yeq243xj/0_jhls3bna
readExpr :: String -> Maybe Expr
readExpr s = case parse exprParser s of
    Just (e, "") -> Just e 
    _            -> Nothing  


{- EBNF
expr = set of term linked by +
term = set of factor linked by *
factor = (expr)

==> it is recursive
-}

exprParser, termParser, factorParser :: Parser Expr
exprParser = foldr1 add <$> chain termParser (char '+')
termParser = foldr1 mul <$> chain factorParser (char '*')
factorParser =
      (char '(' *> exprParser <* char ')')  -- Parentheses <*> is used for ordering parsers and having a resulting one combining all the ops
  <|> (num <$> readsP)                      -- Numbers
  <|> (pure x <$> (char 'x'))               -- Variable "x"
  <|> (char 's' *> char 'i' *> char 'n' *> char '(' *> (sinC <$> exprParser) <* char ')')  -- sin(e)
  <|> (char 'c' *> char 'o' *> char 's' *> char '(' *> (cosC <$> exprParser) <* char ')')  -- cos(e)


-- E
arbExpr :: Int -> Gen Expr -- defining the gen
arbExpr s = frequency [(1,rNum), (1, rX), (s, rBin)]
  where
    range = 100
    rNum = num <$> choose(-range,range)
    rX = do return x -- so the type is Gen Expr
    -- ADD & MUL !! this is where things can go bad (s limit)
    rBin = do
      op <- elements [Add,Mul]
      e1 <- arbExpr $ s `div` 2
      e2 <- arbExpr $ s `div` 2
      return $ e1 `op` e2

instance Arbitrary Expr where -- making the type compatible remind that the 'instance' keyword is for type classes
  arbitrary = sized arbExpr

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = 
     (fromJust (readExpr (showExpr e)) == e) 
  || (fromJust (readExpr (showExpr $ e)) == assoc e) 


assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add (assoc e1) (assoc (Add e2 e3)))
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Sin e) = Sin (assoc e)
assoc (Cos e) = Cos (assoc e)
assoc (Mul (Mul e1 e2) e3) = assoc (Mul (assoc e1) (assoc (Mul e2 e3)))
assoc (Mul e1 e2) = Mul (assoc e1) (assoc e2)
assoc (Num n) = Num n
assoc (X) = X



-- tests : 
ex1 = cosC (sinC (x `add` (num 3.0)))
ex2 = ((num 3) `add` (num 4)) `mul` (num 10)
ex3 = (num 1) `mul` (num 2)

