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
-- Computing the value of a card (10 for higher cards and numeric value for the others)
valueCard :: Card -> Integer
valueCard (Card (Numeric n) s) = n
valueCard (Card r s) = 10

-- Recursively computing the value of a hand by computing the value of each of its cards
value :: Hand -> Integer
value Empty = 0
value (Add c h) = valueCard c + value h