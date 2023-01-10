module Books.Domain.MemberBorrowHistory where

import Books.Domain.BookHistory (BorrowPeriod)
import Books.Domain.ISBN (ISBN)
import Books.Domain.Types

data MemberBorrowHistory = MemberBorrowHistory
  { currentlyBorrowedBooks :: Set CurrentlyBorrowedBook,
    booksBorrowedInThePast :: Set BorrowPeriod
  }

data CurrentlyBorrowedBook = CurrentlyBorrowedBook
  { borrowedAt :: BorrowedAt,
    borrowedBy :: MemberId,
    borrowedBook :: ISBN
  }
