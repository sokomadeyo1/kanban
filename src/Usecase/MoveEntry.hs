module Usecase.MoveEntry (moveEntry) where

import qualified Data.Text as T
import qualified Persistence.Sqlite as Persistence

moveEntry :: Int -> T.Text -> IO (Either T.Text ())
moveEntry entryID colName = do
  Persistence.moveEntry entryID colName
