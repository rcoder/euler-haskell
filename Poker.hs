module Poker ( Suit, Value, Card, Hand, Rank ) where

data Suit = Spade
          | Heart
          | Club
          | Diamond
          deriving (Enum, Eq, Show)

suitToInt :: Suit -> Int
suitToInt s = case s of
  Spade   -> 3
  Heart   -> 2
  Club    -> 1
  Diamond -> 0

instance Ord Suit where 
  compare s1 s2 = compare (suitToInt s1) (suitToInt s2)

charToSuit :: Char -> Suit
charToSuit c = case c of
  'S' -> Spade
  'H' -> Heart
  'C' -> Club
  'D' -> Diamond

data Value = Ace 
           | King 
           | Queen 
           | Jack 
           | Ten 
           | Nine 
           | Eight
           | Seven
           | Six
           | Five
           | Four
           | Three
           | Two
           deriving (Enum, Eq, Show)

valueToInt :: Value -> Int
valueToInt v = case v of
  Ace -> 13
  King -> 12
  Queen -> 11
  Jack -> 10
  Ten -> 9
  Nine -> 8
  Eight -> 7
  Seven -> 6
  Six -> 5
  Five -> 4
  Four -> 3
  Three -> 2
  Two -> 1

instance Ord Value where
  compare v1 v2 = compare (valueToInt v1) (valueToInt v2)

type Card = (Suit, Value)

instance Eq Card where
  (==) (s1, v1) (s2, v2) = (s1 == s2) && (v1 == v2)

cardIntPair :: Card -> (Int, Int)
cardIntPair (s, v) = (suitToInt s, valueToInt v)

instance Ord Card where
  compare c1 c2 = compare (cardIntPair c1) (cardIntPair c2)

data Hand = Hand (Card, Card, Card, Card, Card)

data Rank = RoyalFlush
          | StraightFlush
          | FourOfAKind
          | FullHouse
          | Flush
          | Straight
          | ThreeOfAKind
          | TwoPair
          | Pair
          | HighCard
          deriving (Enum, Eq, Show)

