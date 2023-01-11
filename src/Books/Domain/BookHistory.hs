{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Books.Domain.BookHistory
  ( BookHistory,
    initialHistory,
    HistoryError (..),
    BookEvent (..),
    isbn,
    currentState,
    historicalRecords,
    tryAddEvent,
    BorrowPeriod (..),
    bookEventTime,
    BookState (..),
    historyFromEvents,
  )
where

import Prelude
import Books.Domain.ISBN (ISBN)
import Books.Domain.Types
import Books.Error
import Books.JsonInstances
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Ord as Ord
import Database.Persist.Postgresql (PersistField, PersistFieldSql)
import qualified RIO.List as List
import qualified RIO.Set as Set

data BookHistory = BookHistory
  { isbn :: ISBN,
    currentState :: BookState,
    historicalRecords :: Set BorrowPeriod
  }
  deriving (Show)

initialHistory :: ISBN -> BookHistory
initialHistory isbn = BookHistory isbn Available Set.empty

data HistoryError
  = BookIsNotAvailable
  | CannotReturnBookThatIsNotBorrowed

instance ToAppError HistoryError where
  toAppError BookIsNotAvailable = ValidationError "This book is not available."
  toAppError CannotReturnBookThatIsNotBorrowed = ValidationError "Cannot return a book that hasn't been borrowed."

historyFromEvents :: ISBN -> [BookEvent] -> BookHistory
historyFromEvents isbn =
  foldr
    ( \ev current ->
        case tryAddEvent current ev of
          Right updated -> updated
          -- When building histories from past events
          -- we ignore errors
          Left _err -> current
    )
    (initialHistory isbn)
    . List.sortOn (Ord.Down . bookEventTime)

tryAddEvent :: BookHistory -> BookEvent -> Either HistoryError BookHistory
tryAddEvent state = \case
  Returned returnedAt ->
    case currentState state of
      Available -> Left CannotReturnBookThatIsNotBorrowed
      CurrentlyBorrowed byMember borrowedAt ->
        let historicalRecord = BorrowPeriod borrowedAt returnedAt byMember
         in Right $
              state
                { currentState = Available,
                  historicalRecords = Set.insert historicalRecord (historicalRecords state)
                }
  BorrowedTo memberId borrowedAt ->
    case currentState state of
      CurrentlyBorrowed {} -> Left BookIsNotAvailable
      Available ->
        Right $
          state
            { currentState = CurrentlyBorrowed memberId borrowedAt
            }

-- TASK: Add a smart constructor that prevents you from creating periods
-- with a start date larger than the end date
-- (hint: see the ISBN module)
data BorrowPeriod = BorrowPeriod
  { start :: BorrowedAt,
    end :: ReturnedAt,
    by :: MemberId
  }
  deriving (Show, Eq, Ord)

data BookEvent
  = BorrowedTo MemberId BorrowedAt
  | Returned ReturnedAt
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (PersistFieldSql, PersistField) via (JSONInstances BookEvent)

bookEventTime :: BookEvent -> UTCTime
bookEventTime = \case
  BorrowedTo _ (BorrowedAt time) -> time
  Returned (ReturnedAt time) -> time

data BookState
  = Available
  | CurrentlyBorrowed MemberId BorrowedAt
  deriving (Show)
