{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Column (getColumnR) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.ShowColumn
import Util.Cast (getid)
import Util.PrettyPrint
import Yesod

getColumnR :: T.Text -> Handler Html
getColumnR colname = do
  res <- liftIO $ showColumn $ colname
  case res of
    Left _ -> do
      let entries = [] :: [(Entry, [Tag])]
      defaultLayout $(whamletFile "templates/column.hamlet")
    Right entries -> do
      defaultLayout $(whamletFile "templates/column.hamlet")
