{-# LANGUAGE OverloadedStrings #-}

module Usecase.GetTags (getTags) where

import qualified Data.Text as T
import Domain.Tag
import qualified Persistence.Sqlite as Persistence

getTags :: IO (Either T.Text [Tag])
getTags = do
  tags <- Persistence.getTags
  case tags of
    Right [] -> return $ Left "There are currently no tags. Try using \"NewTag\""
    _ -> return $ tags
