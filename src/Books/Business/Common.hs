module Books.Business.Common where

import Books.Error (AppError, ToAppError (toAppError))
import Control.Monad.Except (MonadError (throwError))
import Books.Prelude

runEither ::
  ToAppError e =>
  MonadError AppError m =>
  Either e a ->
  m a
runEither e =
  case e of
    Left err -> throwError (toAppError err)
    Right result -> pure result
