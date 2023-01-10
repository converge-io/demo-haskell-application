{-# LANGUAGE DefaultSignatures #-}

module Prelude
  ( module X,
    UUID,
    UTCTime,
    nextRandom,
    putStrLn,
    MonadTime (..),
  )
where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import RIO as X hiding (catchIO)
import RIO.Time (UTCTime)
import qualified RIO.Time as Time
import System.IO (putStrLn)

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime
  default getCurrentTime :: MonadIO m => m UTCTime
  getCurrentTime = Time.getCurrentTime