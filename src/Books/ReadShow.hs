{-# LANGUAGE TypeApplications #-}

module Books.ReadShow where

import Prelude
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Sql
import RIO.Text as Text
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (readEither)
import Web.PathPieces

newtype ReadShow a = ReadShow {unReadShow :: a}

instance (Read a, Show a) => PersistField (ReadShow a) where
  toPersistValue = toPersistValue . tshow . unReadShow
  fromPersistValue (PersistText x) = ReadShow <$> mapLeft Text.pack (readEither (Text.unpack x))
  fromPersistValue other = Left $ "Expected text, got " <> tshow other

instance (Read a, Show a) => FromJSON (ReadShow a) where
  parseJSON (String value) =
    ReadShow
      <$> case readMaybe (Text.unpack value) of
        Just val -> pure val
        Nothing -> fail "Could not parse."
  parseJSON other = typeMismatch "String" other

instance (Show a) => ToJSON (ReadShow a) where
  toJSON = String . tshow . unReadShow

instance (Read a, Show a) => PersistFieldSql (ReadShow a) where
  sqlType _ = sqlType $ Proxy @Text

instance (Read a, Show a) => ToHttpApiData (ReadShow a) where
  toUrlPiece = tshow . unReadShow

instance (Read a, Show a) => FromHttpApiData (ReadShow a) where
  parseUrlPiece raw =
    case readEither (Text.unpack raw) of
      Right result -> pure . ReadShow $ result
      Left err -> Left (Text.pack err)

instance (Read a, Show a) => PathPiece (ReadShow a) where
  toPathPiece = tshow . unReadShow
  fromPathPiece input = case readEither (Text.unpack input) of
    Right valid -> pure . ReadShow $ valid
    Left err -> fail err