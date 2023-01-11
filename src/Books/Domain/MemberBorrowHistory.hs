module Books.Domain.MemberBorrowHistory where

import Prelude
import Books.Domain.BookHistory (BorrowPeriod)
import Books.Domain.ISBN (ISBN)
import Books.Domain.Types

data MemberBorrowHistory = MemberBorrowHistory
  { currentlyBorrowedBooks :: Set CurrentlyBorrowedBook,
    booksBorrowedInThePast :: Set BorrowPeriod
  }
  deriving (Show)

data CurrentlyBorrowedBook = CurrentlyBorrowedBook
  { borrowedAt :: BorrowedAt,
    borrowedBy :: MemberId,
    borrowedBook :: ISBN
  }
  deriving (Show, Eq, Ord)
