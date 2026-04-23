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
import Error

colForm :: Html -> MForm Handler (FormResult T.Text, Widget)
colForm = renderDivs $ areq textField "Column Name" Nothing

constrForm :: [T.Text] -> Html -> MForm Handler (FormResult (T.Text, T.Text), Widget)
constrForm cols =
  let
    colList = map (\x -> (x, x)) cols
   in
    renderDivs $
      (,)
        <$> areq (selectFieldList colList) "From" Nothing
        <*> areq (selectFieldList colList) "To"   Nothing

getColumnsR :: Handler Html
getColumnsR = do
  colResult <- liftIO getColumns
  columns <- case colResult of
    Left _ -> return []
    Right columns -> return columns

  constrRes <- liftIO getConstraints
  constraints <- case constrRes of
    Left _ -> return []
    Right constraints -> return constraints

  let colnames = map columnTitle columns
  ((_, widgetCol), enctypeCol) <- runFormPost colForm
  ((_, widgetConstr), enctypeConstr) <- runFormPost $ constrForm colnames

  defaultLayout $(whamletFile "templates/columns.hamlet")

postColumnsR :: Handler Html
postColumnsR = do
  ((formRes, widgetCol), enctypeCol) <- runFormPost colForm
  err <- case formRes of
    FormMissing -> return $ Just ("Error", "Form missing")
    FormFailure e -> return $ Just ("Error", T.append "Form failure: " $ T.show e)
    FormSuccess q -> do
      _ <- liftIO $ newColumn q
      return Nothing

  colRes <- liftIO getColumns
  columns <- case colRes of
    Left _ -> return []
    Right columns -> return columns

  constrRes <- liftIO getConstraints
  constraints <- case constrRes of
    Left _ -> return []
    Right constraints -> return constraints

  let colnames = map columnTitle columns
  ((_, widgetConstr), enctypeConstr) <- runFormPost $ constrForm colnames

  errW <- case err of
    Nothing -> return mempty
    Just (errMsg, errDesc) -> return $ errorWidget errMsg errDesc

  defaultLayout $ errW <> $(whamletFile "templates/columns.hamlet")
