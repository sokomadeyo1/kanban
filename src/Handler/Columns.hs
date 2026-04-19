{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Columns (getColumnsR, postColumnsR) where

import qualified Data.Text as T
import Domain.Column
import Foundation
import Usecase.GetColumns
import Usecase.GetConstraints
import Usecase.NewColumn
import Yesod

postForm :: Html -> MForm Handler (FormResult T.Text, Widget)
postForm = renderDivs $ areq textField "Column Name" Nothing

getColumnsR :: Handler Html
getColumnsR = do
  ((_, widget), enctype) <- runFormPost postForm
  colResult <- liftIO getColumns
  columns <- case colResult of
    Left _ -> return []
    Right columns -> return columns

  constrRes <- liftIO getConstraints
  constraints <- case constrRes of
    Left _ -> return []
    Right constraints -> return constraints

  defaultLayout $(whamletFile "templates/columns.hamlet")

postColumnsR :: Handler Html
postColumnsR = do
  ((formRes, widget), enctype) <- runFormPost postForm
  case formRes of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess q -> do
      _ <- liftIO $ newColumn q
      return ()

  colRes <- liftIO getColumns
  columns <- case colRes of
    Left _ -> return []
    Right columns -> return columns

  constrRes <- liftIO getConstraints
  constraints <- case constrRes of
    Left _ -> return []
    Right constraints -> return constraints

  defaultLayout $(whamletFile "templates/columns.hamlet")
