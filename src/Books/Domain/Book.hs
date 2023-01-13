module Books.Domain.Book where

import Books.Domain.ISBN (ISBN)
import Books.Domain.Types
import Books.Prelude

data Book = Book
  { isbn :: ISBN,
    bookTitle :: BookTitle
  }
  deriving (Show)
