module Usecase.RenameEntry (renameEntry) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

renameEntry :: EntryID -> T.Text -> IO (Either T.Text ())
renameEntry entryid newname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> Persistence.renameEntry entryid newname
