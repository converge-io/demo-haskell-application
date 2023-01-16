{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Books.API.Definition where

import Books.Business.App (App)
import qualified Books.Business.Logic as Logic
import Books.Domain.Book (Book)
import Books.Domain.ISBN (ISBN)
import Books.Domain.Types
import Books.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Servant.API
import Servant.Server (ServerT)

type Auth = BasicAuth "Library" Librarian

type BooksAPI =
  Auth
    :> ( "book" :> ReqBody '[JSON] StockNewBookRequest :> Post '[JSON] Book
           :<|> "book" :> Capture "isbn" ISBN :> "borrowedBy" :> QueryParam' '[Required] "memberId" MemberId :> PutNoContent
           -- TASK: Add an endpoint that marks the book as returned
           -- TASK: Add an endpoint that retrieves a book by its ISBN
       )

server :: ServerT BooksAPI App
server (Librarian _librarianName) =
  stockNewBook
    :<|> markBorrowed
  where
    stockNewBook :: StockNewBookRequest -> App Book
    stockNewBook (StockNewBookRequest title isbn) =
      Logic.newBook isbn title

    markBorrowed :: ISBN -> MemberId -> App NoContent
    markBorrowed isbn memberId =
      Logic.markBorrowed memberId isbn >> pure NoContent

newtype Librarian = Librarian
  { name :: Text
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data StockNewBookRequest = StockNewBookRequest
  { title :: BookTitle,
    isbn :: ISBN
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)
