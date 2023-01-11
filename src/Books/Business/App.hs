{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Books.Business.App where

import Books.Business.Class
import Books.Database.Postgres.Schema
  ( BookEventRow (..),
    BookRow (..),
    CurrentlyBorrowedRow (..),
    EntityField (..),
    Key (BookRowKey, LibraryMemberRowKey, unBookRowKey, unLibraryMemberRowKey),
    LibraryMemberRow (..),
    PastBorrowingRow (..),
    PostgresConnectionPool,
    runDb,
  )
import Books.Domain.Book
import Books.Domain.BookHistory
import Books.Domain.ISBN
import Books.Domain.LibraryMember
import Books.Domain.MemberBorrowHistory (CurrentlyBorrowedBook (..), MemberBorrowHistory (..))
import Books.Domain.Types
import Books.Error (AppError)
import Control.Monad.Except (MonadError (..))
import Data.Has (Has)
import Database.Persist.Postgresql
  ( Entity (Entity, entityVal),
    PersistQueryRead (selectFirst),
    PersistQueryWrite (deleteWhere),
    PersistStoreWrite (insertKey, insertMany_, insert_),
    selectList,
    (==.),
  )
import qualified RIO.Set as Set
import qualified RIO.Time as IO
import Prelude

newtype AppEnv = AppEnv
  { databaseConnectionPool :: PostgresConnectionPool
  }
  deriving (Generic)
  deriving anyclass (Has PostgresConnectionPool)

-- This App monad basically describes the context our app will be running in
-- You can read "ReaderT AppEnv IO" as "a context that has an AppEnv available and can do IO"
newtype App a = App {runApp :: ReaderT AppEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadUnliftIO,
      MonadIO,
      MonadThrow,
      MonadReader AppEnv
    )

instance MonadTime App where
  getCurrentTime = IO.getCurrentTime

instance MonadError AppError App where
  throwError = throwM -- Just using the MonadThrow instance
  catchError = catch -- Just using MonadUnliftIO

-- This is the definition of how our Library will behave under when running in the `App` context
-- You can imagine we could have a `TestApp` that behaved diffetently during tests
instance Library App where
  lookupISBN :: ISBN -> App (Maybe Book)
  lookupISBN isbn = runDb $ do
    -- This is in fact a sql query to the books table and it optionally returns a single row
    bookRowMaybe :: Maybe (Entity BookRow) <- selectFirst [BookRowId ==. BookRowKey isbn] []
    return $ fromBookRow <$> bookRowMaybe

  lookupMemberProfile :: MemberId -> App (Maybe MemberProfile)
  lookupMemberProfile memberId = runDb $ do
    memberRowMaybe <- selectFirst [LibraryMemberRowId ==. LibraryMemberRowKey memberId] []
    case memberRowMaybe of
      Just memberRow -> return . Just . fromMemberRow $ memberRow
      Nothing -> return Nothing

  loadBookHistory :: ISBN -> App (Maybe BookHistory)
  loadBookHistory isbn = runDb $ do
    events <- selectList [BookEventRowBookId ==. BookRowKey isbn] []

    if null events
      then return Nothing
      else
        return . Just
          . historyFromEvents isbn
          . map (bookEventRowEvent . entityVal)
          $ events

  loadMemberHistory :: MemberId -> App MemberBorrowHistory
  loadMemberHistory memberId = runDb $ do
    currentlyBorrowedBooks <- selectList [CurrentlyBorrowedRowMemberId ==. LibraryMemberRowKey memberId] []
    pastBorrowings <- selectList [PastBorrowingRowMemberId ==. LibraryMemberRowKey memberId] []

    return $
      MemberBorrowHistory
        (Set.fromList $ map fromCurrentlyBorrowedRow currentlyBorrowedBooks)
        (Set.fromList $ map fromPastBorrowingRow pastBorrowings)

  stockNewBook :: ISBN -> BookTitle -> App Book
  stockNewBook isbn title = runDb $ do
    insertKey (BookRowKey isbn) (BookRow title)
    return $ Book isbn title

  registerMember :: MemberName -> App MemberProfile
  registerMember name = runDb $ do
    memberId <- MemberId <$> liftIO nextRandom
    insertKey (LibraryMemberRowKey memberId) (LibraryMemberRow name)
    return $ MemberProfile memberId name

  recordBookEvent :: ISBN -> BookEvent -> App ()
  recordBookEvent isbn newEvent = runDb $ do
    -- This runs in a single transaction due to how runDb operates
    -- 1. Create a database row and store the event
    insert_ $ BookEventRow newEvent (BookRowKey isbn)
    -- 2. Get the past events and construct the new history
    -- This can be seen as inefficient although it will do
    -- perfectly fine for our toy application
    -- TASK: If you want, explore ways of optimizing this
    -- (hint: maybe some sort of snapshots)
    pastEvents <- selectList [BookEventRowBookId ==. BookRowKey isbn] []
    let newHistory = historyFromEvents isbn (newEvent : map (bookEventRowEvent . entityVal) pastEvents)
    -- 3. Delete old history data
    -- We will just drop everything and repopulate it, but feel free
    -- to optimize these queries as much as you wish
    deleteWhere [PastBorrowingRowBookId ==. BookRowKey isbn]
    deleteWhere [CurrentlyBorrowedRowBookId ==. BookRowKey isbn]
    -- 4. Project the updated history so we can query our tables
    case currentState newHistory of
      Available ->
        -- We don't need to project anything in
        -- the currently_borrowed table if the book is available
        return ()
      CurrentlyBorrowed memberId borrowedAt ->
        insert_ $
          CurrentlyBorrowedRow
            (LibraryMemberRowKey memberId)
            (BookRowKey isbn)
            borrowedAt

    insertMany_
      ( map
          ( \(BorrowPeriod start end by) ->
              PastBorrowingRow start end (BookRowKey isbn) (LibraryMemberRowKey by)
          )
          . Set.toList
          . historicalRecords
          $ newHistory
      )

fromBookRow :: Entity BookRow -> Book
fromBookRow (Entity bookRowId bookRow) =
  Book
    (unBookRowKey bookRowId)
    (bookRowTitle bookRow)

fromMemberRow :: Entity LibraryMemberRow -> MemberProfile
fromMemberRow (Entity memberRowId row) =
  MemberProfile
    (unLibraryMemberRowKey memberRowId)
    (libraryMemberRowName row)

fromCurrentlyBorrowedRow :: Entity CurrentlyBorrowedRow -> CurrentlyBorrowedBook
fromCurrentlyBorrowedRow (Entity _ row) =
  CurrentlyBorrowedBook
    (currentlyBorrowedRowBorrowedAt row)
    (unLibraryMemberRowKey . currentlyBorrowedRowMemberId $ row)
    (unBookRowKey . currentlyBorrowedRowBookId $ row)

fromPastBorrowingRow :: Entity PastBorrowingRow -> BorrowPeriod
fromPastBorrowingRow (Entity _ row) =
  BorrowPeriod
    (pastBorrowingRowBorrowedAt row)
    (pastBorrowingRowReturnedAt row)
    (unLibraryMemberRowKey . pastBorrowingRowMemberId $ row)
