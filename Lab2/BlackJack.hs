module BlackJack where

-- imports
import Cards
import RunGame
import Test.QuickCheck

-- A0
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add(Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            ,2]


-- A1
-- Display a card depending on its rank
displayCard :: Card -> String
displayCard (Card (Numeric n) s) = show n ++ " of " ++ show s
displayCard (Card r s)           = show r ++ " of " ++ show s

-- Recusively displaying hand by displaying its cards
display :: Hand -> String
display Empty       = ""
display (Add c h) = displayCard c ++ "\n" ++ display h


-- A2
-- Computing the value of a card (10 for higher cards, 11 for ace,  
-- numeric value for the others)
valueCard :: Card -> Integer
valueCard (Card (Numeric n) s) = n
valueCard (Card (Ace r) s) = 11
valueCard (Card r s) = 10

-- Recursively computing the value of a hand by computing 
-- the value of each of its cards
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueCard c + value h

-- Recursively computing the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Ace c) h) = 1 + numberOfAces h
numberOfAces (Add c h) = numberOfAces h 

-- Using the last two functions to compute the acutal value of the hand 
-- (depending of the 21 score)
value :: Hand -> Integer
value hand | score < 21 = score 
           | nbAces > 0 = score -10
           | otherwise = score
           where score = initialValue hand
                 nbAces = numberOfAces hand