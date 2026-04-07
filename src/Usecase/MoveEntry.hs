module Usecase.MoveEntry (moveEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Column
import qualified Persistence.Sqlite as Persistence

moveEntry :: EntryID -> T.Text -> IO (Either T.Text ())
moveEntry entryid colname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      col <- Persistence.getOneColumn colname
      case col of
        Left err -> return $ Left err
        Right (Column colid _) -> Persistence.moveEntry entryid colid
