{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Books.Domain.Types where

-- TASK: Introduce a type NonEmptyText and force that BookTitles are always non-empty
-- (hint: NonEmptyText should use a smart constructor to prevent empty values)
newtype BookTitle = BookTitle Text

newtype BorrowedAt = BorrowedAt UTCTime
  deriving newtype (Eq, Ord)

newtype ReturnedAt = ReturnedAt UTCTime
  deriving newtype (Eq, Ord)

newtype MemberId = MemberId UUID
  deriving newtype (Eq, Ord)
