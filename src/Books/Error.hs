module Books.Error where

import Books.Prelude

data AppError
  = ValidationError Text
  | NotFound Text

class ToAppError e where
  toAppError :: e -> AppError

instance ToAppError AppError where
  toAppError = id
