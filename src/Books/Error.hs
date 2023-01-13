module Books.Error where

import Books.Prelude
import qualified RIO.Text as T

data AppError
  = ValidationError Text
  | NotFound Text

instance Show AppError where
  show = \case
    ValidationError t -> T.unpack t
    NotFound t -> T.unpack t

instance Exception AppError

class ToAppError e where
  toAppError :: e -> AppError

instance ToAppError AppError where
  toAppError = id
