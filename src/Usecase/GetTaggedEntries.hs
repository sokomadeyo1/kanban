{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetTaggedEntries (getTaggedEntries) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence
import Usecase.GetEntries

getEntriesTags :: Entry -> IO (Either T.Text [Tag])
getEntriesTags entry = do
  let entryid = entryID entry
  result <- Persistence.getOneEntry entryid
  case result of
    Left err -> return $ Left err
    Right _ -> do
      tags <- Persistence.getEntriesTags entryid
      return tags

getTaggedEntries :: IO (Either T.Text [(Entry, [Tag])])
getTaggedEntries = do
  result <- getEntries
  case result of
    Left err -> return $ Left err
    -- TODO: find out about 
    Right entries -> do
      eithertags <- mapM getEntriesTags entries
      let tags = map filterRight eithertags
      return $ Right $ zip entries tags

filterRight :: (Either a [b]) -> [b]
filterRight (Left _) = []
filterRight (Right x) = x
