{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Books.Database.Postgres.Schema where

import Data.Has
import Books.Prelude
import Books.Domain.BookHistory (BookEvent)
import Books.Domain.ISBN (ISBN)
import Books.Domain.Types (BookTitle, BorrowedAt, MemberId, MemberName, ReturnedAt)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, runSqlPool)

share
  [mkPersist sqlSettings, mkMigrate "migrateBooksDb"]
  [persistLowerCase|
BookRow sql=books
  Id ISBN
  title BookTitle
BookEventRow sql=book_events
  event BookEvent -- An example of how we can use the JSON instance to store JSON in our database
  bookId BookRowId -- This will automatically become a foreign key to BookRow
LibraryMemberRow sql=library_members
  Id MemberId
  name MemberName
-- These will be our 'query tables'.
-- The book events are the source of truth, but our application
-- needs some way of querying history of borrowings.
-- We will populate these tables as we record new events.
-- This is a mini-example of the concept of event-sourcing.
CurrentlyBorrowedRow sql=currently_borrowed_books
  memberId LibraryMemberRowId
  bookId BookRowId
  borrowedAt BorrowedAt
PastBorrowingRow sql=past_borrowings
  borrowedAt BorrowedAt
  returnedAt ReturnedAt
  bookId BookRowId
  memberId LibraryMemberRowId
|]

newtype PostgresConnectionPool = PostgresConnectionPool { connPool :: ConnectionPool }

-- If we have a PostgresConnectionPool within
-- our context then we can execute database operations
runDb ::
  forall r m a.
  Has PostgresConnectionPool r =>
  MonadReader r m =>
  MonadUnliftIO m =>
  SqlPersistT m a ->
  -- ^ A database operation in persistent
  m a
runDb action = do
  connectionPool <- asks (connPool . getter)
  -- This will run in a single transaction
  runSqlPool action connectionPool