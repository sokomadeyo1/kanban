{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Tag (getTagR, postTagR) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.RenameTag
import Usecase.GetEntriesByTag
import Util.Cast (getid)
import Util.PrettyPrint
import Yesod
import Error

tagFormGen :: T.Text -> Html -> MForm Handler (FormResult T.Text, Widget)
tagFormGen colname = renderDivs $ areq textField "Tag name" (Just $ colname)

getTagR :: T.Text -> Handler Html
getTagR tagname = do
  res <- liftIO $ getEntriesByTag tagname
  let formGen = tagFormGen tagname
  ((_, widget), enctype) <- runFormPost formGen
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries
  defaultLayout $(whamletFile "templates/tag.hamlet")

postTagR :: T.Text -> Handler Html
postTagR tagname_ = do
  let formGen = tagFormGen tagname_
  ((formRes, widget), enctype) <- runFormPost formGen
  (tagname, err) <- case formRes of
    FormMissing -> return $ (tagname_, Just ("Error", "Form missing"))
    FormFailure e -> return $ (tagname_, Just ("Error", T.append "Form failure: " $ T.show e))
    FormSuccess newname -> do
      renameRes <- liftIO $ renameTag tagname_ newname
      case renameRes of
        Left e -> return $ (tagname_, Just ("Error", e))
        Right _ -> return $ (newname, Nothing)

  res <- liftIO $ getEntriesByTag tagname_
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries

  errW <- case err of
    Nothing -> return mempty
    Just (errMsg, errDesc) -> return $ errorWidget errMsg errDesc
  defaultLayout $ errW <> $(whamletFile "templates/tag.hamlet")
