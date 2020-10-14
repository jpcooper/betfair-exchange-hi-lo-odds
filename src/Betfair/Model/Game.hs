module Betfair.Model.Game
  ( Card(..)
  , Game(Game)
  , Board(NoCards, Cards)
  , Odds(Odds)
  , Amount(Amount)
  , OddsAmount(OddsAmount)
  , Selection(Selection)
  , MarketId(MarketId)
  , MarketStatus(Active, Settled, SuspendedGameRoundOver, SuspendedGameSettling)
  , SelectionStatus(InPlay, Winner)
  , formatOdds
  , fromPlayedCards
  , getNumberLower
  , getNumberRemaining
  , remaining
  , lastPlayed
  , toBack
  , toLay
  , marketId
  , windowTime
  , windowPercentageComplete
  , board
  , selections
  , marketStatus
  , selectionStatus
  ) where

import           Prelude
  ( Show
  , Enum
  , Eq
  , Ord
  , Rational
  , Int
  , String
  , (*)
  , (==)
  , fromEnum
  , quotRem
  , show
  , shows
  , take
  , toEnum
  , undefined
  )

import           Data.Function (($))
import           Data.List ((++), last)
import           Data.Ratio (denominator , numerator)

import           Data.Set (Set)
import qualified Data.Set as Set (difference, fromList, size, split)
import           Data.Text (Text)

import           Data.Vector (Vector)

decimalPlacesOdds :: Int
decimalPlacesOdds = 2

data Card = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Show, Ord)

instance Enum Card where
  fromEnum Two = 40
  fromEnum Three = 41
  fromEnum Four = 42
  fromEnum Five = 43
  fromEnum Six = 44
  fromEnum Seven = 45
  fromEnum Eight = 46
  fromEnum Nine = 47
  fromEnum Ten = 48
  fromEnum Jack = 49
  fromEnum Queen = 50
  fromEnum King = 51
  fromEnum Ace = 39

  toEnum 40 = Two
  toEnum 41 = Three
  toEnum 42 = Four
  toEnum 43 = Five
  toEnum 44 = Six
  toEnum 45 = Seven
  toEnum 46 = Eight
  toEnum 47 = Nine
  toEnum 48 = Ten
  toEnum 49 = Jack
  toEnum 50 = Queen
  toEnum 51 = King
  toEnum 39 = Ace
  toEnum _ = undefined

data Game = Game
  { marketId :: MarketId
  , windowTime :: Int
  , windowPercentageComplete :: Int
  , marketStatus :: MarketStatus
  , board :: Board
  , selections :: Vector Selection
  } deriving Show

data MarketStatus = Active
                  | SuspendedGameRoundOver
                  | SuspendedGameSettling
                  | Settled

instance Show MarketStatus where
  show Active = "ACTIVE"
  show SuspendedGameRoundOver = "SUSPENDED_GAME_ROUND_OVER"
  show SuspendedGameSettling = "SUSPENDED_GAME_SETTLING"
  show Settled = "SETTLED"

newtype MarketId = MarketId Text
  deriving Show

data Board = NoCards
           | Cards { remaining :: Set Card, lastPlayed :: Card }
  deriving Show

newtype Odds = Odds Rational

formatRational :: Rational -> Int -> String
formatRational rational decimalPlaces =
  if decimalPlaces == 0
  then show wholePart
  else shows wholePart ("." ++ take decimalPlaces (go remainder))

  where (wholePart, remainder) = num `quotRem` den
        num = numerator rational
        den = denominator rational

        go 0 = '0' : go 0
        go x = let (d, next) = (10 * x) `quotRem` den
               in shows d (go next)


formatOdds :: Odds -> Int -> String
formatOdds (Odds odds) decimalPlaces =
  formatRational odds decimalPlaces

instance Show Odds where
  show (Odds odds) = formatRational odds decimalPlacesOdds

newtype Amount = Amount Rational

instance Show Amount where
  show (Amount amount) = formatRational amount 0

data OddsAmount = OddsAmount Odds Amount

instance Show OddsAmount where
  show (OddsAmount odds amount) =
    show odds ++ ":" ++ show amount

data Selection = Selection
  { selectionStatus :: SelectionStatus
  , toBack :: [OddsAmount]
  , toLay :: [OddsAmount]
  }
  deriving Show

data SelectionStatus = InPlay
                     | Winner
  deriving (Eq, Show)

allCards :: Set Card
allCards = Set.fromList [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

fromPlayedCards :: [Card] -> Board
fromPlayedCards [] =
  NoCards
fromPlayedCards nonEmpty =
  Cards (Set.difference allCards $ Set.fromList nonEmpty) $ last nonEmpty

getNumberLower :: Board -> Int
getNumberLower NoCards =
  0
getNumberLower Cards {..} =
  Set.size lower

  where (lower, _) = Set.split lastPlayed remaining

getNumberRemaining :: Board -> Int
getNumberRemaining NoCards =
  Set.size allCards
getNumberRemaining Cards {..} =
  Set.size remaining
