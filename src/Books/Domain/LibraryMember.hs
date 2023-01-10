module Books.Domain.LibraryMember where

import Books.Domain.MemberBorrowHistory (MemberBorrowHistory)
import Books.Domain.Types

data MemberProfile = MemberProfile
  { memberId :: MemberId,
    history :: MemberBorrowHistory
  }
