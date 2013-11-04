module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck

-- size hand2
-- = size (Add (Card (Numeric 2) Hearts)
-- (Add (Card Jack Spades) Empty))
-- = ... =2

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
-- valueCard (Card r _) = valueRank r   (I think this style is consistent with the rest of the code, and more readable, you agree?)

-- Number of Aces in a Hand

numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand  -- stop when we reach 4 aces ??
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

-- If the bank or player is the winner

winner :: Hand -> Hand -> Player
winner ghand _     | gameOver ghand = Bank
winner ghand bhand | value bhand    >= value ghand = Bank
                   | otherwise      = Guest


-- Puts a hand on top of another one

(<+):: Hand -> Hand -> Hand
h1 <+ h2 = (reverseHand h1 Empty) <++ h2
  where
    (<++):: Hand -> Hand -> Hand
    Empty           <++ h2 = h2 
    (Add card hand) <++ h2 = hand <++ (Add card h2) 


-- Test if the function (<+) is associative

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test if the size of the combined hands is equal to
-- the sum of the size of individual hands 

prop_size_onTopOf :: Hand -> Hand -> Bool 
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1<+h2)

-- Reverses a hand

reverseHand :: Hand -> Hand -> Hand
reverseHand  Empty h2          = h2
reverseHand (Add card hand) h2 = reverseHand hand (Add card h2) 


-- Test the reverse function

prop_reverseHand :: Hand -> Bool
prop_reverseHand h = h == (reverseHand (reverseHand h Empty) Empty)

-- Returns a hand of cards of the same suit.

giveHand :: Suit -> Integer -> Hand -> Hand
giveHand    s  14 h             = Add Card {rank = Ace , suit = s} h
giveHand    s  13 h             = giveHand s (14) (Add Card {rank = King , suit = s} h)
giveHand    s  12 h             = giveHand s (13) (Add Card {rank = Queen , suit = s} h)
giveHand    s  11 h             = giveHand s (12) (Add Card {rank = Jack , suit = s} h)
giveHand    s  n  h | (n < 11)  = giveHand s (n+1) (Add Card {rank = Numeric n , suit = s} h)


-- Returns a deck of cards.

fullDeck :: Hand
fullDeck =(giveHand Hearts 2 Empty) <+ (giveHand Spades 2 Empty) <+
          (giveHand Clubs 2 Empty)  <+ (giveHand Diamonds 2 Empty)
		  
		  
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
