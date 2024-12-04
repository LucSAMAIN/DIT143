module Expr where


import Prelude hiding (sin, cos) -- Hides sin and cos from unqualified usage
import qualified Prelude as P   -- Allows using P.sin and P.cos explicitly
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
  deriving (Show, Eq)




-- instance Show Expr where
--   show = showExpr

-- Simple functions to construct exprs
x :: Expr
x = X

num :: Double -> Expr
num = Num

add :: Expr -> Expr -> Expr
add = Add

mul :: Expr -> Expr -> Expr
mul = Mul

sin :: Expr -> Expr
sin = Sin

cos :: Expr -> Expr -- C stands for constructor handle name issue with our functions
cos = Cos

size :: Expr -> Integer
size (Num value) = 0
size X = 0
size (Sin e) = 1 + size e
size (Cos e) = 1 + size e
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2



-- B
showExpr :: Expr -> String
showExpr (Num value) = show value
showExpr X = "x"
showExpr (Sin e) = "sin" ++ showParensIfNeeded e
showExpr (Cos e) = "cos" ++ showParensIfNeeded e
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

-- Helper function to add parentheses for arguments when needed
showParensIfNeeded :: Expr -> String
showParensIfNeeded (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"  -- Parentheses required for `Add`
showParensIfNeeded (Mul e1 e2) = "(" ++ showExpr (Mul e1 e2) ++ ")"  -- Parentheses required for `Mul`
showParensIfNeeded e = " " ++ showExpr e                             -- No parentheses for single terms

-- Helper function for multiplication arguments
showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showFactor e = showExpr e








-- C
eval :: Expr -> Double -> Double
eval (Num value) _ = value
eval X xvalue = xvalue
eval (Sin e) xvalue = P.sin $ eval e xvalue
eval (Cos e) xvalue = P.cos $ eval e xvalue
eval (Add e1 e2) xvalue = (eval e1 xvalue) + eval e2 xvalue
eval (Mul e1 e2) xvalue = eval e1 xvalue * (eval e2 xvalue)



-- D see videos from lecture 5A https://play.chalmers.se/playlist/dedicated/0_yeq243xj/0_jhls3bna
readExpr :: String -> Maybe Expr
readExpr s = case parse exprParser s of
    Just (e, "") -> Just (assoc e) 
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
  <|> (pure x <$> char 'x')               -- Variable "x"
  <|> (char 's' *> char 'i' *> char 'n' *> char '(' *> (sin <$> exprParser) <* char ')')  -- sin(e)
  <|> (char 'c' *> char 'o' *> char 's' *> char '(' *> (cos <$> exprParser) <* char ')')  -- cos(e)


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
  || (fromJust (readExpr (showExpr e)) == assoc e) 


assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add (assoc e1) (assoc (Add e2 e3)))
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Sin e) = Sin (assoc e)
assoc (Cos e) = Cos (assoc e)
assoc (Mul (Mul e1 e2) e3) = assoc (Mul (assoc e1) (assoc (Mul e2 e3)))
assoc (Mul e1 e2) = Mul (assoc e1) (assoc e2)
assoc (Num n) = Num n
assoc X = X







-- F
simplify :: Expr -> Expr
simplify (Add e1 e2) =
    case (simplify e1, simplify e2) of
      (Num v1, Num v2)             -> Num (v1 + v2) 
      (Num 0, simplifiedE2)        -> simplifiedE2 
      (simplifiedE1, Num 0)        -> simplifiedE1  
      (simplifiedE1, simplifiedE2) -> Add simplifiedE1 simplifiedE2 -- otherwise


simplify (Mul e1 e2) =
    case (simplify e1, simplify e2) of
      (Num v1, Num v2)      -> Num (v1 * v2)
      (Num 0, _)            -> Num 0 
      (_, Num 0)            -> Num 0 
      (Num 1, simplifiedE2) -> simplifiedE2 
      (simplifiedE1, Num 1) -> simplifiedE1   
      (simplifiedE1, simplifiedE2) -> Mul simplifiedE1 simplifiedE2 -- otherwise


simplify (Sin e) =
    case simplify e of
        Num v -> Num (P.sin v)  -- If the simplified expression is a number, compute the sine
        simplifiedE -> Sin simplifiedE  -- Otherwise, keep it as Sin with the simplified expression

simplify (Cos e) =
    case simplify e of
        Num v -> Num (P.cos v)  
        simplifiedE -> Cos simplifiedE  

simplify (Num v) = (Num v)
simplify X     = X

-- Simplification correctness property
prop_simplify_correctness :: Expr -> Double -> Bool
prop_simplify_correctness expr xVal =
  eval expr xVal == eval (simplify expr) xVal

-- Helper function to validate that an expression is fully simplified
isSimplified :: Expr -> Bool
isSimplified (Add (Num _) (Num _)) = False  -- Addition of two numbers should be simplified
isSimplified (Mul (Num _) (Num _)) = False  -- Multiplication of two numbers should be simplified
isSimplified (Add (Num 0) _) = False       -- No addition with 0
isSimplified (Add _ (Num 0)) = False       -- No addition with 0
isSimplified (Mul (Num 0) _) = False       -- No multiplication by 0
isSimplified (Mul _ (Num 0)) = False       -- No multiplication by 0
isSimplified (Mul (Num 1) _) = False       -- No multiplication by 1
isSimplified (Mul _ (Num 1)) = False       -- No multiplication by 1
isSimplified (Sin (Num _))   = False       -- No sin(num)
isSimplified (Cos (Num _))   = False       -- No cos(num)
isSimplified (Add e1 e2)     = isSimplified e1 && isSimplified e2
isSimplified (Mul e1 e2)     = isSimplified e1 && isSimplified e2
isSimplified (Sin e)         = isSimplified e
isSimplified (Cos e)         = isSimplified e
isSimplified (Num _)         = True        -- Numeric constants are fine
isSimplified X               = True        -- Variable is fine


-- Simplified expressions should have no redundancies
prop_simplify_no_redundancies :: Expr -> Bool
prop_simplify_no_redundancies expr =
  isSimplified (simplify expr)

-- quickCheck prop_simplify_correctness
-- quickCheck prop_simplify_no_redundancies



-- G
differentiate :: Expr -> Expr
differentiate e =  simplify $ differentiateCompute e

differentiateCompute :: Expr -> Expr
differentiateCompute (Add e1 e2) = (differentiate e1) `add` (differentiate e2)
differentiateCompute (Mul e1 e2) = ((differentiate e1) `mul` e2) `add` 
                                 (e1 `mul` (differentiate e2))
differentiateCompute (Sin e)     = ((differentiate e) `mul` (Cos e))
differentiateCompute (Cos e)     = (differentiate e) `mul` ((Num (-1.0)) `mul` (Sin e))
differentiateCompute (Num v)     = (Num 0)
differentiateCompute X           = (Num 1)






  

-- -- tests : 
-- ex1 = cos (sin (x `add` (num 3.0)))
-- ex2 = ((num 3) `add` (num 4)) `mul` (num 10)
-- ex3 = (num 1) `mul` (num 2)

-- -- tests for simplify:
-- exs :: [Expr]
-- exs = [ (num 1) `mul` (num 0),             -- 0
--         (num 0) `mul` (num 2),             -- 0
--         (num 1) `add` (num 0),             -- 1
--         (num 0) `add` (num 1),             -- 1
--         (num 3) `mul` (num 4),             -- itself
--         (num 0) `add` (num 0),             -- 0
--         (x `add` (num 0)),                 -- itself
--         (num 0) `mul` x,                   -- 0
--         sin (num 0),                      -- Sine of zero (result should remain sin (num 0) unless further simplifications are defined)
--         cos (num 0),                      -- Cosine of zero (result should remain cos (num 0) unless further simplifications are defined)
--         (x `add` (num 2)) `mul` (num 0),   -- Multiplication of an expression by zero (result should be 0)
--         sin (x `add` (num 0)),             -- Sine of an addition with zero (result should simplify the addition first)
--         sin (num (15.3)),
--         cos (num (15.3))
--       ]