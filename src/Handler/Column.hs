{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Column (getColumnR, postColumnR) where

import qualified Data.Text as T
import Domain.Entry
import Error
import Foundation
import Usecase.RenameColumn
import Usecase.ShowColumn
import Util.Cast (getid)
import Util.PrettyPrint
import Yesod

columnFormGen :: T.Text -> Html -> MForm Handler (FormResult T.Text, Widget)
columnFormGen colname = renderDivs $ areq textField "Column name" (Just $ colname)

getColumnR :: T.Text -> Handler Html
getColumnR colname = do
  res <- liftIO $ showColumn $ colname
  let formGen = columnFormGen colname
  ((_, widget), enctype) <- runFormPost formGen
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries
  defaultLayout $(whamletFile "templates/column.hamlet")

postColumnR :: T.Text -> Handler Html
postColumnR colname_ = do
  res <- liftIO $ showColumn $ colname_
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries

  let formGen = columnFormGen colname_
  ((formRes, widget), enctype) <- runFormPost formGen
  (colname, err) <- case formRes of
    FormMissing -> return (colname_, Just ("Error", "Form missing"))
    FormFailure e -> return (colname_, Just ("Error", T.append "Form failure: " $ T.show e))
    FormSuccess newname -> do
      renameRes <- liftIO $ renameColumn colname_ newname
      return $ case renameRes of
        Left e -> (colname_, Just ("Error", e))
        Right _ -> (newname, Nothing)

  errW <- case err of
    Nothing -> return mempty
    Just (errMsg, errDesc) -> return $ errorWidget errMsg errDesc
  defaultLayout $ errW <> $(whamletFile "templates/column.hamlet")
