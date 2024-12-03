import Parsing



-- A, see videos: https://play.chalmers.se/playlist/dedicated/0_bmaetfz1/0_n6gvtmek (lecture 4A)
data Expr
  = Num Double         -- Use Double for numbers as per the question
  | Add Expr Expr      -- Addition
  | Mul Expr Expr      -- Multiplication
  | Sin Expr           -- Sinus
  | Cos Expr           -- Cosinus
  | X                  -- Variable "x"
  deriving (Eq)

instance Show Expr where
  show = showExpr

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
    Just (e, "") -> Just e  -- Successfully parsed the entire input
    _            -> Nothing  -- Parsing failed or didn't consume the full input

-- Parser for `Expr`
exprParser :: Parser Expr
exprParser = chain termParser (char '+' *> pure Add)

-- Parser for terms (multiplication has higher precedence than addition)
termParser :: Parser Expr
termParser = chain factorParser (char '*' *> pure Mul)

-- Parser for factors (parentheses, numbers, variables, sin, cos)
factorParser :: Parser Expr
factorParser =
      (char '(' *> exprParser <* char ')')  -- Parentheses
  <|> (Num <$> readsP)                      -- Numbers
  <|> (char 'x' *> pure X)                  -- Variable "x"
  <|> (char 's' *> char 'i' *> char 'n' *> char '(' *> (Sin <$> exprParser) <* char ')')  -- sin(e)
  <|> (char 'c' *> char 'o' *> char 's' *> char '(' *> (Cos <$> exprParser) <* char ')')  -- cos(e)

-- Helper function for chaining terms (e.g., a*b*c)
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >*> rest
  where
    rest x = (op >*> \f -> p >*> \y -> rest (f x y)) +++ pure x










-- tests : 
ex1 = cosC (sinC (x `add` (num 3.0)))
ex2 = ((num 3) `add` (num 4)) `mul` (num 10)
ex3 = (num 1) `mul` (num 2)

