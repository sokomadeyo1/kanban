{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Columns (getColumnsR) where

import Domain.Column
import Foundation
import Usecase.GetColumns
import Util.PrettyPrint
import Yesod

getColumnsR :: Handler Html
getColumnsR = defaultLayout $ do
  result <- liftIO getColumns
  case result of
    Left _ -> [whamlet||]
    Right columns -> $(whamletFile "templates/columns.hamlet")
