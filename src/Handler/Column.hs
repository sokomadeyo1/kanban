{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Column (getColumnR, postColumnR) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
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
  case res of
    Left _ -> do
      let entries = [] :: [(Entry, [Tag])]
      defaultLayout $(whamletFile "templates/column.hamlet")
    Right entries -> do
      defaultLayout $(whamletFile "templates/column.hamlet")

postColumnR :: T.Text -> Handler Html
postColumnR colname_ = do
  res <- liftIO $ showColumn $ colname_
  case res of
    Left _ -> defaultLayout $ [whamlet||]
    Right entries -> do
      let formGen = columnFormGen colname_
      ((formRes, widget), enctype) <- runFormPost formGen
      case formRes of
        FormMissing -> defaultLayout [whamlet||]
        FormFailure _ -> defaultLayout [whamlet||]
        FormSuccess newname -> do
          renameRes <- liftIO $ renameColumn colname_ newname
          case renameRes of
            Left _ -> defaultLayout [whamlet||]
            Right _ -> do
              let colname = newname
              defaultLayout $(whamletFile "templates/column.hamlet")
