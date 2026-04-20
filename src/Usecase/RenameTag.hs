{-# LANGUAGE OverloadedStrings #-}

module Usecase.RenameTag (renameTag) where

import qualified Data.Text as T
import Domain.Tag
import qualified Persistence.Sqlite as Persistence
import Util.PrettyPrint
import Util.Class (dummy)

renameTag :: T.Text -> T.Text -> IO (Either T.Text ())
renameTag oldname newname = do
  checkCollision <- Persistence.getOneTag newname
  case checkCollision of
    Right _ -> return $ Left $ T.unwords ["Tag", pretty (dummy newname :: Tag), "already exists"]
    Left _ -> do
      tag <- Persistence.getOneTag oldname
      case tag of
        Left err -> return $ Left err
        Right (Tag tagid _) -> Persistence.renameTag tagid newname
