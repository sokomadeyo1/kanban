module Usecase.MoveEntry (moveEntry) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

moveEntry :: EntryID -> T.Text -> IO (Either T.Text ())
moveEntry entryID colName = do
  entry <- Persistence.getOneEntry entryID
  case entry of
    Left err -> return $ Left err
    Right _ -> Persistence.moveEntry entryID colName
