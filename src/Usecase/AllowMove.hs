{-# LANGUAGE OverloadedStrings #-}

module Usecase.AllowMove (allowMove) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence
import Util.Class
import Util.PrettyPrint

allowMove :: T.Text -> T.Text -> IO (Either T.Text ())
allowMove fromcolname tocolname = do
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
            Right False -> return $ Left $ T.unwords
              [ "Moving from"
              , pretty $ (dummy fromcolname :: Column)
              , "to"
              , pretty $ (dummy tocolname :: Column)
              , "is not restricted"
              ]
            Right True -> Persistence.allowMove fromcolid tocolid
