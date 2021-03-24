{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Betfair.Game
  ( Card(..)
  , Game(Game)
  , Board(NoCards, Cards)
  , Odds(Odds)
  , Amount(Amount, getAmount)
  , OddsAmount(OddsAmount, getAmount)
  , Selection(Selection)
  , MarketId(MarketId)
  , MarketStatus(Active, Settled, SuspendedGameRoundOver, SuspendedGameSettling)
  , Round(Round)
  , SelectionId(SelectionId)
  , SelectionStatus(InPlay, Winner, Loser)
  , fromPlayedCards
  , getMarketId
  , getNumberLower
  , getNumberRemaining
  , getRound
  , getOdds
  , remaining
  , lastPlayed
  , toBack
  , toLay
  , marketId
  , round
  , windowTime
  , windowPercentageComplete
  , board
  , selections
  , marketStatus
  , selectionId
  , selectionStatus
  ) where

import           Prelude hiding (round)

import           Data.Set (Set)
import qualified Data.Set as Set (difference, fromList, size, split)
import           Data.Text (Text)

import           Betfair.Serialisation (formatRational)

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
  { round :: Round
  , marketId :: MarketId
  , windowTime :: Int
  , windowPercentageComplete :: Int
  , marketStatus :: MarketStatus
  , board :: Board
  , selections :: [Selection]
  } deriving Show

data MarketStatus = Active
                  | SuspendedGameRoundOver
                  | SuspendedGameSettling
                  | Settled
  deriving Eq

instance Show MarketStatus where
  show Active = "ACTIVE"
  show SuspendedGameRoundOver = "SUSPENDED_GAME_ROUND_OVER"
  show SuspendedGameSettling = "SUSPENDED_GAME_SETTLING"
  show Settled = "SETTLED"

newtype Round = Round {getRound :: Int}
  deriving (Eq, Ord, Show)

newtype MarketId = MarketId {getMarketId :: Text}
  deriving (Eq, Ord, Show)

data Board = NoCards
           | Cards { remaining :: Set Card, lastPlayed :: Card }
  deriving Show

newtype Odds = Odds Rational

instance Show Odds where
  show (Odds odds) = formatRational odds decimalPlacesOdds

newtype Amount = Amount {getAmount :: Rational}

instance Show Amount where
  show (Amount amount) = formatRational amount 0

data OddsAmount = OddsAmount {getOdds :: Odds, getAmount :: Amount}

instance Show OddsAmount where
  show (OddsAmount odds amount) =
    show odds ++ ":" ++ show amount

newtype SelectionId = SelectionId String
  deriving (Eq, Ord, Show)

data Selection = Selection
  { selectionId :: SelectionId
  , selectionStatus :: SelectionStatus
  , toBack :: [OddsAmount]
  , toLay :: [OddsAmount]
  }
  deriving Show

data SelectionStatus = InPlay
                     | Winner
                     | Loser
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
