{-# LANGUAGE OverloadedStrings #-}

module Usecase.MoveEntry (moveEntry) where

import qualified Data.Text as T
import Domain.Column
import Domain.Entry
import qualified Persistence.Sqlite as Persistence
import Util.Class
import Util.PrettyPrint

moveEntry :: EntryID -> T.Text -> IO (Either T.Text ())
moveEntry entryid tocolname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right (Entry _ _ _ fromcolname) -> do
      col <- Persistence.getOneColumn tocolname
      case col of
        Left err -> return $ Left err
        Right (Column tocolid _) -> do
          fromcol <- Persistence.getOneColumn fromcolname
          case fromcol of
            Left err -> return $ Left err
            Right (Column fromcolid _) -> do
              checkRestrict <- Persistence.checkRestrict fromcolid tocolid
              case checkRestrict of
                Left err -> return $ Left err
                Right False -> Persistence.moveEntry entryid tocolid
                Right True -> return $ Left $ T.unwords
                  [ "Moving from"
                  , pretty $ (dummy fromcolname :: Column)
                  , "to"
                  , pretty $ (dummy tocolname :: Column)
                  , "is restricted"
                  ]
