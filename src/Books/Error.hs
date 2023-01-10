module Books.Error where

data AppError
  = ValidationError Text
  | NotFound Text

class ToAppError e where
  toAppError :: e -> AppError

instance ToAppError AppError where
  toAppError = id
