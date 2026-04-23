{-# LANGUAGE OverloadedStrings #-}

module Usecase.RestrictMove (restrictMove) where

import qualified Data.Text as T
import Domain.Column
import qualified Persistence.Sqlite as Persistence
import Util.PrettyPrint
import Util.Class

restrictMove :: T.Text -> T.Text -> IO (Either T.Text ())
restrictMove fromcolname tocolname = do
  if fromcolname == tocolname
    then return $ Left $ "Matching column names"
    else do
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
                  , pretty $ (dummy fromcolname :: Column)
                  , "to"
                  , pretty $ (dummy tocolname :: Column)
                  , "is already restricted"
                  ]
                Right False -> Persistence.restrictMove fromcolid tocolid
