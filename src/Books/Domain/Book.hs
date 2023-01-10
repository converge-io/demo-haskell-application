module Books.Domain.Book where

import Books.Domain.ISBN (ISBN)
import Books.Domain.Types

data Book = Book
  { isbn :: ISBN,
    bookTitle :: BookTitle
  }
