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

hand3 = Add (Card Ace Spades) (Add (Card Ace Clubs) Empty)
hand4 = Add (Card Ace Spades) (Add (Card Ace Clubs) (Add (Card Ace Hearts) Empty))
hand5 = Add(Card Ace Spades) (Add (Card King Clubs) Empty)
hand6 = Add(Card King Spades) (Add (Card Queen Clubs) (Add (Card Jack Hearts) Empty))
hand7 = Add(Card King Spades) (Add (Card (Numeric 6) Hearts) Empty)

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
valueCard (Card Ace s) = 11
valueCard (Card r s) = 10

-- Recursively computing the value of a hand by computing 
-- the value of each of its cards
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueCard c + value h

-- Recursively computing the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add c h) = numberOfAces h 

-- Using the last two functions to compute the acutal value of the hand 
-- (depending of the 21 score)
value :: Hand -> Integer
value hand | score < 22 = score 
            -- the score is too high, can we decrease it using aces?
           | nbAces > 0 && (score-10) < 21 = score + (-10)
           | nbAces > 1 && (score-20) < 22 = score + (-20)
           | nbAces > 2 && (score-30) < 22 = score + (-30)
           | nbAces > 3 && (score-40) < 22 = score + (-40)
           | otherwise = score
           where score = initialValue hand
                 nbAces = numberOfAces hand

-- A3
gameOver :: Hand -> Bool
gameOver hand = value hand > 21