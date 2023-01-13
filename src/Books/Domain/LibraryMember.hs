module Books.Domain.LibraryMember where

import Books.Domain.Types
import Books.Prelude

data MemberProfile = MemberProfile
  { memberId :: MemberId,
    name :: MemberName
  }
  deriving (Show)
