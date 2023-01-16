{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Books.Domain.Book where

import Books.Domain.ISBN (ISBN)
import Books.Domain.Types
import Books.Prelude
import Data.Aeson

data Book = Book
  { isbn :: ISBN,
    bookTitle :: BookTitle
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
