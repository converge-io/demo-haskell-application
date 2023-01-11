module Books.Domain.LibraryMember where

import Books.Domain.Types
import Prelude (Show)

data MemberProfile = MemberProfile
  { memberId :: MemberId,
    name :: MemberName
  }
  deriving (Show)
