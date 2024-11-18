module BlackJack where

-- imports
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- A0
hand2 :: Hand
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

hand3 :: Hand
hand4 :: Hand
hand5 :: Hand
hand6 :: Hand
hand7 :: Hand

hand3 = Add (Card Ace Spades) (Add (Card Ace Clubs) Empty)
hand4 = Add (Card Ace Spades) (Add (Card Ace Clubs) (Add (Card Ace Hearts) Empty))
hand5 = Add(Card Ace Spades) (Add (Card King Clubs) Empty)
hand6 = Add(Card King Spades) (Add (Card Queen Clubs) (Add (Card Jack Hearts) Empty))
hand7 = Add(Card King Spades) (Add (Card (Numeric 6) Hearts) Empty)

-- A1
-- Display a card depending on its rank
displayCard :: Card -> String
displayCard (Card (Numeric n) s)| s == Hearts =  show n ++ " \9829"
                                | s == Spades =  show n ++ " \9824"
                                | s == Diamonds =  show n ++ " \9830"
                                | s == Clubs =  show n ++ " \9827"

displayCard (Card r s)  | s == Hearts =  show r ++ " \9829"
                        | s == Spades =  show r ++ " \9824"
                        | s == Diamonds =  show r ++ " \9830"
                        | s == Clubs =  show r ++ " \9827"

-- Recusively displaying hand by displaying its cards
display :: Hand -> String
display Empty       = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- A2
-- Computing the value of a card (10 for higher cards, 11 for ace,  
-- numeric value for the others)
valueCard :: Card -> Integer
valueCard (Card (Numeric n) _) = n
valueCard (Card Ace _) = 11
valueCard (Card _ _) = 10

-- Recursively computing the value of a hand by computing 
-- the value of each of its cards
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueCard c + initialValue h

-- Recursively computing the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h) = numberOfAces h 

-- Using the last two functions to compute the actual value of the hand
-- Adjusting value if it exceeds 21 and there are aces
value :: Hand -> Integer
value hand
    | score <= 21 = score
    | otherwise = score - 10 * nbAces
    where 
        score = initialValue hand
        nbAces = numberOfAces hand

-- A3
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4
-- See "2 - The game" to understand how the rules work here
winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver handGuest && gameOver handBank = Bank
                          | gameOver handGuest = Bank
                          | gameOver handBank = Guest
                          -- None of the hands have value > 21
                          | value handGuest > value handBank = Guest
                          | otherwise = Bank -- if draw then bank wins anyway haha

-- B1 
(<+) :: Hand -> Hand -> Hand
Empty <+ Empty = Empty
Empty <+ h2 = h2
h1 <+ Empty = h1
(Add c h1) <+ h2 = Add c (h1 <+ h2)


-- linked properties for Quicheck on B1:
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size p1 + size p2 == size (p1<+p2)

-- B2
-- Generate all cards of a given suit
suitHand :: Suit -> Hand
suitHand s = foldr Add Empty [Card r s | r <- allRanks]

-- List of all ranks
allRanks :: [Rank]
allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

-- Generate a full deck by combining all suit hands
fullDeck :: Hand
fullDeck = suitHand Hearts <+ suitHand Spades <+ suitHand Diamonds <+ suitHand Clubs

-- B3
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c rest) h1 = (rest, (Add c h1))

-- B4
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck bankHand | value bankHand < 16 = playBankHelper smallerDeck biggerHand
                             | otherwise = bankHand
    where (smallerDeck, biggerHand) = draw deck bankHand

playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- B5
removeNthCard :: Integer -> Hand -> (Hand, Card) -- takes nth card, deck and returns (new deck, removed card)
removeNthCard i deck    | i >= size deck = error "removeNthCard: index cannot be superior to the size of the deck."
                        | i < 0         = error "removeNthCard : index cannot be negative."
removeNthCard i (Add c deck) | i == 0 = (deck, c)
                             | otherwise = ((Add c newDeck), removedCard)
                            where (newDeck, removedCard) = removeNthCard (i-1) deck

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty  
shuffleDeck g deck = shuffleHelper g deck Empty

shuffleHelper :: StdGen -> Hand -> Hand -> Hand
shuffleHelper _ Empty shuffled = shuffled 
shuffleHelper g deck shuffled = 
    let (index, g1) = randomR (0, (size deck) - 1) g  
        (newDeck, card) = removeNthCard index deck  
    in shuffleHelper g1 newDeck (Add card shuffled) 


-- properties linked to B5:
belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool -- to do
prop_size_shuffle g deck = (size (shuffleDeck g deck)) == (size deck)


-- B6
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO () 
main = runGame implementation