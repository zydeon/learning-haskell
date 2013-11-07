module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck


-- ========================== lab 2A =================

-- size hand2
-- = size (Add (Card (Numeric 2) Hearts)
--             (Add (Card Jack Spades) Empty))
-- = 1 + size (Add (Card Jack Spades) Empty)
-- = 1 + (1 + size Empty)
-- = 1 + 1 + 0
-- = 2


-- Return empty Hand

empty :: Hand
empty = Empty

-- Value of a Rank according to if the player Hand is bust (Bool)
 
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11  -- "Initially the value eleven is used for the aces"
valueRank _           = 10

-- Value of a Card

valueCard :: Card -> Integer
valueCard c = valueRank (rank c)

-- Number of Aces in a Hand

numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add (Card _ _) hand)   = numberOfAces hand

-- Value of a Hand

value :: Hand -> Integer
value h | v > 21    = v - ((numberOfAces h) * 10)
        | otherwise = v
        where v = value' h

-- Temporary value of Hand with aces counting with default value

value' :: Hand -> Integer
value' Empty           = 0
value' (Add card hand) = (valueCard card) + (value' hand)

-- If a player is bust

gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21

-- If the bank (bhand) or guest (ghand) is the winner

winner :: Hand -> Hand -> Player
winner ghand _     | gameOver ghand = Bank
winner _     bhand | gameOver bhand = Guest
winner ghand bhand | value bhand    >= value ghand = Bank
                   | otherwise      = Guest


-- ========================== end of lab 2A =================


-- Puts a hand on top of another one

(<+):: Hand -> Hand -> Hand
Empty         <+ h = h
(Add card h') <+ h = Add card (h' <+ h)

-- Test if the function (<+) is associative

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test if the size of the combined hands is equal to
-- the sum of the size of individual hands 

prop_size_onTopOf :: Hand -> Hand -> Bool 
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1<+h2)

-- Returns a hand of cards of the same suit.
        
giveHand :: Suit -> Integer -> Hand 
giveHand s 14 = Add Card {rank = Ace , suit = s} Empty
giveHand s 13 = Add Card {rank = King , suit = s} (giveHand s 14)
giveHand s 12 = Add Card {rank = Queen , suit = s} (giveHand s 13)
giveHand s 11 = Add Card {rank = Jack , suit = s} (giveHand s 12)           
giveHand s n 
         |(n<11) && (n>1)  = Add Card {rank = Numeric n, suit = s} (giveHand s (n+1))
         |otherwise        = error "givehand: invalid value!"
 

-- Returns a deck of cards.

fullDeck :: Hand
fullDeck =(giveHand Hearts 2) <+ (giveHand Spades 2) <+
          (giveHand Clubs 2)  <+ (giveHand Diamonds 2)
      
      
-- Tests the size of a full deck of cards.      
  
prop_sizeTest :: Bool
prop_sizeTest = size fullDeck == 52


-- Draw Card from deck and put on hand.
-- Return (deck,hand)

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand           = error "draw: The deck is empty."  -- change this later
draw (Add card deck) hand = (deck,Add card hand)


{-implementation = Interface   Should be completed after all the functions are done!
 {
 iEmpty    = empty ,
 iFullDeck = fullDeck ,
 iValue    = value ,
 iGameOver = gameOver ,
 iWinner   = winner,
 iDraw     = draw
 }

main :: IO ()
main = runGame implementation -}