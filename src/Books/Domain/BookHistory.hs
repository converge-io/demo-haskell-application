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
  )
where

import Books.Domain.ISBN (ISBN)
import Books.Domain.Types
import Books.Error
import qualified RIO.Set as Set

data BookHistory = BookHistory
  { isbn :: ISBN,
    currentState :: BookState,
    historicalRecords :: Set BorrowPeriod
  }

initialHistory :: ISBN -> BookHistory
initialHistory isbn = BookHistory isbn Available Set.empty

data HistoryError
  = BookIsNotAvailable
  | CannotReturnBookThatIsNotBorrowed

instance ToAppError HistoryError where
  toAppError BookIsNotAvailable = ValidationError "This book is not available."
  toAppError CannotReturnBookThatIsNotBorrowed = ValidationError "Cannot return a book that hasn't been borrowed."

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
  deriving (Eq, Ord)

data BookEvent
  = BorrowedTo MemberId BorrowedAt
  | Returned ReturnedAt

data BookState
  = Available
  | CurrentlyBorrowed MemberId BorrowedAt
