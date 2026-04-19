{-# LANGUAGE OverloadedStrings #-}

module Handler.Constr (postConstraintR) where

import qualified Data.Text as T
import Domain.Column
import Foundation
import Usecase.GetColumns
import Usecase.RestrictMove
import Yesod

constrForm :: [T.Text] -> Html -> MForm Handler (FormResult (T.Text, T.Text), Widget)
constrForm cols =
  let
    colList = map (\x -> (x, x)) cols
   in
    renderDivs $
      (,)
        <$> areq (selectFieldList colList) "From" Nothing
        <*> areq (selectFieldList colList) "To"   Nothing

postConstraintR :: Handler ()
postConstraintR = do
  colRes <- liftIO getColumns
  columns <- case colRes of
    Left _ -> return []
    Right columns -> return columns
  let colnames = map columnTitle columns
  ((res, _), _) <- runFormPost $ constrForm colnames

  case res of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess (from, to) -> do
      _ <- liftIO $ restrictMove from to
      return ()

  redirect ColumnsR
