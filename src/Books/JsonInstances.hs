{-# LANGUAGE ScopedTypeVariables #-}

module Books.JsonInstances where

import Books.Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Database.Persist.Sql
import qualified RIO.Text as Text

newtype JSONInstances a = JSONInstances {unJSONInstances :: a}

instance (ToJSON a) => ToJSON (JSONInstances a) where
  toJSON = toJSON . unJSONInstances

instance (FromJSON a) => FromJSON (JSONInstances a) where
  parseJSON val = JSONInstances <$> parseJSON val

instance (ToJSON a, FromJSON a) => PersistFieldSql (JSONInstances a) where
  sqlType _ = SqlOther "jsonb"

instance (ToJSON a, FromJSON a) => PersistField (JSONInstances a) where
  toPersistValue = PersistLiteralEscaped . LBS.toStrict . encode
  fromPersistValue = \case
    (PersistLiteralEscaped bs) -> decodeFromBS bs
    (PersistByteString bs) -> decodeFromBS bs
    other -> Left $ "Expected PersistDBSpecific, but got " <> tshow other <> " instead."

decodeFromBS ::
  FromJSON a =>
  ByteString ->
  Either Text a
decodeFromBS bs =
  case eitherDecodeStrict bs of
    Right x ->
      Right x
    Left err ->
      Left $ "Could not deserialize from db column: " <> Text.pack err
