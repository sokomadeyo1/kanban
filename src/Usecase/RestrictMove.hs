module Usecase.RestrictMove (restrictMove) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence

restrictMove :: T.Text -> T.Text -> IO (Either T.Text ())
restrictMove fromcolname tocolname = do
  fromcol <- Persistence.getOneColumn fromcolname
  case fromcol of
    Left err -> return $ Left err
    Right (Column fromcolid _) -> do
      tocol <- Persistence.getOneColumn tocolname
      case tocol of
        Left err -> return $ Left err
        Right (Column tocolid _) -> Persistence.restrictMove fromcolid tocolid
