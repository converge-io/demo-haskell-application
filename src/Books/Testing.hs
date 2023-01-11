module Books.Testing where

import Books.Business.App (App (..), AppEnv (..))
import qualified Books.Business.Class as Logic
import qualified Books.Business.Logic as Logic
import Books.Database.Postgres.Schema (PostgresConnectionPool (..), migrateBooksDb)
import Books.Domain.ISBN (mkISBN, ISBN)
import Books.Domain.Types (BookTitle (..), MemberId (..))
import Control.Monad.Logger (NoLoggingT (..))
import qualified Data.Maybe as Partial
import qualified Data.UUID as UUID
import Database.Persist.Postgresql (PostgresConf (..), createPostgresqlPoolWithConf, defaultPostgresConfHooks, runMigration, runSqlPool)
import Text.Pretty.Simple (pPrint)
import Prelude

-- A module we can load in the REPL to test our application so far

getAppEnv :: IO AppEnv
getAppEnv = do
  let connString = "host=localhost port=6543 user=postgres password=postgres dbname=books"
      postgresConf = PostgresConf connString 1 8 8
  pool <- runNoLoggingT $ createPostgresqlPoolWithConf postgresConf defaultPostgresConfHooks

  -- Run the database migrations
  runMigration migrateBooksDb `runSqlPool` pool

  return $ AppEnv (PostgresConnectionPool pool)

appInIO :: AppEnv -> App a -> IO a
appInIO env = flip runReaderT env . runApp

isbn :: ISBN
(Right isbn) = mkISBN "978-3-16-148410-0"

isbn1 :: ISBN
(Right isbn1) = mkISBN "960-425-059-0"

testMemberId :: MemberId
testMemberId = MemberId (Partial.fromJust . UUID.fromText $ "775df88c-37fb-4d5d-81cf-e225fd1060b1")

testStuff :: IO ()
testStuff = do
  env <- getAppEnv
  let title = BookTitle "a great book"
  appInIO env $ do
    -- You can execute your business logic here
    book <- Logic.newBook isbn title `catch` (\(_ex :: SomeException) -> Partial.fromJust <$> Logic.lookupISBN isbn)
    book1 <- Logic.newBook isbn1 title `catch` (\(_ex :: SomeException) -> Partial.fromJust <$> Logic.lookupISBN isbn1)
    member <- Partial.fromJust <$> Logic.lookupMemberProfile testMemberId

    pPrint book
    pPrint member

    -- Test borrow twice
    -- Logic.borrowBook memberId isbn
    -- Logic.returnBook isbn
    Logic.borrowBook testMemberId isbn

    bookHistory <- Logic.loadBookHistory isbn
    memberHistory <- Logic.loadMemberHistory testMemberId

    pPrint bookHistory
    pPrint memberHistory

    return ()
