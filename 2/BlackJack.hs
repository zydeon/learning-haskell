module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
import System.Random


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
         |otherwise        = error "giveHand: invalid value!"
 

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
draw Empty hand           = error "draw: The deck is empty."
draw (Add card deck) hand = (deck,Add card hand)


-- Draws card for the bank player

playBank' :: Hand -> Hand -> Hand
playBank' deck hand |(value hand >= 16) = hand
                    | otherwise         = playBank' newDeck  newHand
                       where (newDeck , newHand) = draw deck hand

-- Tests the final value of bank hand

prop_draw' :: Bool
prop_draw' = value (playBank' fullDeck Empty) >= 16

-- Plays for the bank and returns the bank's final hand.
 
playBank :: Hand -> Hand
playBank bh = playBank' (shuffle (mkStdGen 1) fullDeck) bh
 

-- Whether a card is in the deck or not.

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


-- Tests whether a card is in the deck after and before shuffling the deck.

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h 
  

-- returns n-th card from hand and the resulting hand

nthcard :: Integer -> Hand -> (Card,Hand)
nthcard _ Empty      = error "nthcard: invalid number of card to return!"
nthcard n _ | n <= 0 = error "nthcard: invalid number of card to return!"
nthcard 1 (Add c h)  = (c, h)
nthcard n (Add c h)  = (c', Add c h')
            where (c',h') = nthcard (n-1) h

-- Shuffles a deck of cards (stdgen, old deck, new deck)

shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g old   = Add card (shuffle g' old')
    where   (r, g')      = randomR (1,size old) g
            (card, old') = nthcard r old

			
-- Tests whether size of the deck is preserved by shuffle.

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle (mkStdGen 1) h)


implementation = Interface 
 {
 iEmpty    = empty ,
 iFullDeck = fullDeck ,
 iValue    = value ,
 iGameOver = gameOver ,
 iWinner   = winner ,
 iDraw     = draw ,
 iPlayBank = playBank ,
 iShuffle  = shuffle
 }

main :: IO ()
main = runGame implementation
