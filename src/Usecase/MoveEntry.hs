module Usecase.MoveEntry (moveEntry) where

import qualified Persistence.Sqlite as Persistence
import qualified Data.Text as T

moveEntry :: Int -> T.Text -> IO (Either String ())
moveEntry entryID colName = do
  Persistence.moveEntry entryID colName
