{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Books.Main where

import Books.API.Definition (BooksAPI)
import Books.API.Definition qualified as API
import Books.Business.App (App, AppEnv (..), appInIO)
import Books.Database.Postgres.Schema (PostgresConnectionPool (PostgresConnectionPool), migrateBooksDb)
import Books.Error
import Books.Prelude
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Logger (NoLoggingT (..))
import Data.Aeson (ToJSON)
import Data.Aeson qualified as JSON
import Data.CaseInsensitive (mk)
import Database.Persist.Postgresql (PostgresConf (..), createPostgresqlPoolWithConf, defaultPostgresConfHooks, runMigration, runSqlPool)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Network.Wai.Middleware.Servant.Errors (errorMw)

runApp :: IO ()
runApp = do
  appEnv <- getAppEnv

  let context = checkBasicAuthCredentials :. EmptyContext
      waiApplication = serveWithContext (Proxy @BooksAPI) context (server appEnv)

  run 7777 . logStdoutDev . errorMw @JSON @'["message", "status"] $ waiApplication
  where
    server :: AppEnv -> Server BooksAPI
    server env =
      hoistServerWithContext
        (Proxy @BooksAPI)
        (Proxy @'[BasicAuthCheck API.Librarian])
        (appToHandler env)
        API.server

    checkBasicAuthCredentials :: BasicAuthCheck API.Librarian
    checkBasicAuthCredentials = BasicAuthCheck $ \(BasicAuthData user pass) -> do
      if user == "converge" && pass == "rocks"
        then pure . Authorized $ API.Librarian "converge-librarian"
        else pure NoSuchUser

    appToHandler :: AppEnv -> App a -> Servant.Handler a
    appToHandler env =
      Servant.Handler
        . ExceptT
        . try
        . rethrow toServantError
        . appInIO env

    rethrow :: (MonadUnliftIO m, MonadThrow m, Exception a1, Exception e) => (a1 -> e) -> IO a2 -> m a2
    rethrow transformError operation = do
      liftIO operation `catch` (throwM . transformError)

    toServantError ::
      AppError ->
      ServerError
    toServantError (ValidationError msg) = servantErrorWithText err400 msg
    toServantError (NotFound msg) = servantErrorWithText err404 msg

    servantErrorWithText ::
      ServerError ->
      Text ->
      ServerError
    servantErrorWithText sErr msg =
      sErr
        { errBody = errorBody (errHTTPCode sErr),
          errHeaders = [(mk "Content-Type", "application/json;charset=utf-8")]
        }
      where
        errorBody code = JSON.encode $ GenericApiError msg code

data GenericApiError = GenericApiError
  { message :: Text,
    status :: Int
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

getAppEnv :: IO AppEnv
getAppEnv = do
  -- TASK: Read the connection string from the environment
  let connString = "host=localhost port=6543 user=postgres password=postgres dbname=books"
      postgresConf = PostgresConf connString 1 8 8
  pool <- runNoLoggingT $ createPostgresqlPoolWithConf postgresConf defaultPostgresConfHooks

  -- Run the database migrations
  runMigration migrateBooksDb `runSqlPool` pool

  return $ AppEnv (PostgresConnectionPool pool)