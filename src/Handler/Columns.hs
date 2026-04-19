{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Columns (getColumnsR, postColumnsR) where

import qualified Data.Text as T
import Domain.Column
import Foundation
import Usecase.GetColumns
import Yesod
import Usecase.NewColumn

postForm :: Html -> MForm Handler (FormResult T.Text, Widget)
postForm = renderDivs $ areq textField "Column Name" Nothing

getColumnsR :: Handler Html
getColumnsR = do
  ((_, widget), enctype) <- runFormPost postForm
  result <- liftIO getColumns
  case result of
    Left _ -> defaultLayout [whamlet||]
    Right columns -> defaultLayout $(whamletFile "templates/columns.hamlet")

postColumnsR :: Handler Html
postColumnsR = do
  ((formRes, widget), enctype) <- runFormPost postForm
  case formRes of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess q -> do
      _ <- liftIO $ newColumn q
      return ()
  result <- liftIO getColumns
  case result of
    Left _ -> defaultLayout [whamlet||]
    Right columns -> defaultLayout $(whamletFile "templates/columns.hamlet")
