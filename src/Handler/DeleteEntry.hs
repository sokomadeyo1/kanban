{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.DeleteEntry (postDeleteEntryR) where

import Domain.Entry
import Foundation
import Usecase.DeleteEntry
import Yesod

postDeleteEntryR :: Int -> Handler Html
postDeleteEntryR i = do
  _ <- liftIO $ deleteEntry $ EntryID i
  redirect BoardR
