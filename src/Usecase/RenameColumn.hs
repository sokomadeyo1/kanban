module Usecase.RenameColumn (renameColumn) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence

renameColumn :: T.Text -> T.Text -> IO (Either T.Text ())
renameColumn oldname newname = do
  col <- Persistence.getOneColumn oldname
  case col of
    Left err -> return $ Left err
    Right (Column colid _) -> Persistence.renameColumn colid newname
