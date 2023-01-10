module Books.Business.Logic where

import Books.Business.Class (Library (..))
import Books.Business.Common (runEither)
import Books.Domain.Book (Book)
import Books.Domain.BookHistory (BookEvent (..), BookHistory, initialHistory, tryAddEvent)
import Books.Domain.ISBN (ISBN, renderISBN)
import Books.Domain.Types (BookTitle, BorrowedAt (..), MemberId, ReturnedAt (..))
import Books.Error (AppError (..))
import Control.Monad.Except (MonadError (throwError))

-- Note on `MonadError AppError`:
-- Using `MonadError` over `MonadThrow` can be seen as a controversial decision.
-- In "Exceptions Best Practices" Michael Snoyman recommends using `MonadThrow`
-- because that gives you more composability (you can throw any errors everywhere)
-- and it's also "in like with Haskell's runtime exception system" which does not
-- capture the types of exceptions that can be thrown.
-- (there are also other arguments which involve async exceptions but I'll not get into those)
-- These arguments are valid but as with anything, using `MonadThrow` is a tradeoff.
-- For our intents and purposes `MonadError AppError` can be seen as better because
-- 1 it is more beginner friendly
-- 2 it prevents us from accidentaly throwing other types of errors
-- 3 (peeking into the future) eventually we'll have a "catch all" block
-- on our API which will transform AppError into a nice API response

-- TASK: The domain model currently assumes there can only be 1 instance of a book
-- in the whole library. Make it so we can stock multiple books with the same ISBN and lend them out.
-- (hint: look at BookHistory)
borrowBook ::
  MonadError AppError m =>
  Library m =>
  MonadTime m =>
  MemberId ->
  ISBN ->
  m ()
borrowBook borrowedBy toBorrow = do
  now <- BorrowedAt <$> getCurrentTime
  void $ updateBookHistory toBorrow (BorrowedTo borrowedBy now)

returnBook ::
  MonadError AppError m =>
  Library m =>
  MonadTime m =>
  ISBN ->
  m ()
returnBook toReturn = do
  now <- ReturnedAt <$> getCurrentTime
  void $ updateBookHistory toReturn (Returned now)

updateBookHistory ::
  MonadError AppError m =>
  Library m =>
  ISBN ->
  BookEvent ->
  m BookHistory
updateBookHistory isbn ev = do
  currentHistory <- getBookHistory isbn
  newHistory <- runEither (tryAddEvent currentHistory ev)
  recordBookEvent isbn ev
  pure newHistory

getBookHistory ::
  MonadError AppError m =>
  Library m =>
  ISBN ->
  m BookHistory
getBookHistory isbn = do
  -- TASK: This code requires quite a lot of nesting
  -- and checks. Play around with ways to make it better.
  book <- lookupISBN isbn
  case book of
    Just _ -> do
      history <- lookupBookHistory isbn
      case history of
        Just h -> pure h
        Nothing -> do
          -- If the book exists but we didn't find any history for it
          -- that likely means that no events have happened for that
          -- book and we can just return `initialHistory`.
          pure $ initialHistory isbn
    Nothing -> throwError $ NotFound $ "Book with isbn " <> renderISBN isbn <> " was not found."

-- TASK: This one is a bit more advanced but might be fun to explore
-- Currently we need to check manually whether the ISBN exists in the database
-- and throw an error if we're duplicating a book.
-- Try making it so we can't forget to call `lookupISBN`
-- (hint: we can add some type information to the ISBN type like `ISBN 'Existing`
-- explore the DataKinds extension and possibly GADTs (although you won't necessarily need them))
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html
-- https://dnikolovv.github.io/practical-haskell-ddd-gadt/
newBook ::
  Library m =>
  MonadError AppError m =>
  ISBN ->
  BookTitle ->
  m Book
newBook isbn title = do
  existing <- lookupISBN isbn
  case existing of
    Just _alreadyExists -> throwError $ ValidationError "Book with this ISBN already exists."
    Nothing -> stockNewBook isbn title
