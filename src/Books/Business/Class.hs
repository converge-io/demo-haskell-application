module Books.Business.Class where

import Books.Domain.Book (Book)
import Books.Domain.BookHistory (BookEvent, BookHistory)
import Books.Domain.ISBN (ISBN)
import Books.Domain.LibraryMember (MemberProfile)
import Books.Domain.MemberBorrowHistory (MemberBorrowHistory)
import Books.Domain.Types (BookTitle, MemberId, MemberName)
import Books.Prelude

class Monad m => Library m where
  lookupISBN :: ISBN -> m (Maybe Book)
  loadBookHistory :: ISBN -> m (Maybe BookHistory)
  loadMemberHistory :: MemberId -> m MemberBorrowHistory
  lookupMemberProfile :: MemberId -> m (Maybe MemberProfile)
  registerMember :: MemberName -> m MemberProfile
  stockNewBook :: ISBN -> BookTitle -> m Book
  recordBookEvent :: ISBN -> BookEvent -> m ()
