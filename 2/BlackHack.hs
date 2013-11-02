module BlackJack where
import Cards
import Wrapper

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
value' Empty = 0
value' (Add card hand) = (valueCard card) + (value' hand)

-- If a player is bust

gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21

-- Draw Card from deck and put on hand.
-- Return (deck,hand)

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand           = error "draw: The deck is empty."  -- change this later
draw (Add card deck) hand = (deck,Add card hand)

