module Usecase.MoveEntry (moveEntry) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

moveEntry :: EntryID -> T.Text -> IO (Either T.Text ())
moveEntry entryID colName = do
  Persistence.moveEntry entryID colName
