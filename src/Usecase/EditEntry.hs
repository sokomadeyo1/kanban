module Usecase.EditEntry (editEntry) where

import qualified Data.Text as T
import Domain.Entry
import qualified Persistence.Sqlite as Persistence

editEntry :: EntryID -> T.Text -> IO (Either T.Text ())
editEntry entryid newname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> Persistence.editEntry entryid newname
