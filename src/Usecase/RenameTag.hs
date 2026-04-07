module Usecase.RenameTag (renameTag) where

import qualified Data.Text as T
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

renameTag :: T.Text -> T.Text -> IO (Either T.Text ())
renameTag oldname newname = do
  tag <- Persistence.getOneTag oldname
  case tag of
    Left err -> return $ Left err
    Right (Tag tagid _) -> Persistence.renameTag tagid newname
