{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- Don't issue a warning because we have an orphan UUID instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Books.Domain.Types where

import Books.Prelude
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.ByteString.Char8 as B8
import qualified Data.UUID as UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
  ( PersistField (..),
    PersistFieldSql (..),
    PersistValue (..),
    SqlType (..),
  )
import Servant.API (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece (fromPathPiece, toPathPiece))

-- TASK: Introduce a type NonEmptyText and force that BookTitles are always non-empty
-- (hint: NonEmptyText should use a smart constructor to prevent empty values)
newtype BookTitle = BookTitle Text
  deriving newtype (Show, ToJSON, FromJSON, PersistFieldSql, PersistField)

newtype BorrowedAt = BorrowedAt UTCTime
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, PersistFieldSql, PersistField)

newtype ReturnedAt = ReturnedAt UTCTime
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, PersistFieldSql, PersistField)

newtype MemberName = MemberName Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, PersistFieldSql, PersistField)

newtype MemberId = MemberId UUID
  deriving newtype
    ( Eq,
      Ord,
      Read,
      Show,
      PathPiece,
      FromHttpApiData,
      ToHttpApiData,
      ToJSON,
      FromJSON,
      PersistFieldSql,
      PersistField
    )

-- The uuid library doesn't have a built-in notion of how to translate UUIDs to database types
-- thus we define it here. These "orphan" instances are sometimes frowned upon
-- but for our purposes this is more than fine.
instance PersistField UUID where
  toPersistValue = PersistLiteralEscaped . B8.pack . UUID.toString
  fromPersistValue (PersistLiteralEscaped t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDbSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = UUID.toText
  fromPathPiece = UUID.fromText
