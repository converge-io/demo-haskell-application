module Books.Business.Class where

import Books.Domain.Book (Book)
import Books.Domain.BookHistory (BookEvent, BookHistory)
import Books.Domain.ISBN (ISBN)
import Books.Domain.LibraryMember (MemberProfile)
import Books.Domain.Types (BookTitle, MemberId)
import Books.Prelude

class Monad m => Library m where
  lookupISBN :: ISBN -> m (Maybe Book)
  lookupBookHistory :: ISBN -> m (Maybe BookHistory)
  lookupMemberProfile :: MemberId -> m (Maybe MemberProfile)
  stockNewBook :: ISBN -> BookTitle -> m Book
  recordBookEvent :: ISBN -> BookEvent -> m ()
