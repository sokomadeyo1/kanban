{-# LANGUAGE OverloadedStrings #-}

module Usecase.RestrictMove (restrictMove) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence
import PrettyPrint

restrictMove :: T.Text -> T.Text -> IO (Either T.Text ())
restrictMove fromcolname tocolname = do
  fromcol <- Persistence.getOneColumn fromcolname
  case fromcol of
    Left err -> return $ Left err
    Right (Column fromcolid _) -> do
      tocol <- Persistence.getOneColumn tocolname
      case tocol of
        Left err -> return $ Left err
        Right (Column tocolid _) -> do
          check <- Persistence.checkRestrict fromcolid tocolid
          case check of
            Left err -> return $ Left err
            Right True -> return $ Left $ T.unwords
              [ "Moving from"
              , pretty $ Column (ColumnID 0) fromcolname
              , "to"
              , pretty $ Column (ColumnID 0) tocolname
              , "is already restricted"
              ]
            Right False -> Persistence.restrictMove fromcolid tocolid
