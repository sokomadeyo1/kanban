{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Tags (getTagsR, postTagsR) where

import qualified Data.Text as T
import Domain.Tag
import Foundation
import Usecase.GetTags
import Usecase.NewTag
import Yesod
import Util.PrettyPrint
import Error

tagForm :: Html -> MForm Handler (FormResult T.Text, Widget)
tagForm = renderDivs $ areq textField "Tag name" Nothing

getTagsR :: Handler Html
getTagsR = do
  tagResult <- liftIO getTags
  tags <- case tagResult of
    Left _ -> return []
    Right tags -> return tags

  ((_, widget), enctype) <- runFormPost tagForm

  defaultLayout $(whamletFile "templates/tags.hamlet")

postTagsR :: Handler Html
postTagsR = do
  ((formRes, widget), enctype) <- runFormPost tagForm
  err <- case formRes of
    FormMissing -> return $ Just ("Error", "Form missing")
    FormFailure e -> return $ Just ("Error", T.append "Form failure: " $ T.show e)
    FormSuccess q -> do
      _ <- liftIO $ newTag q
      return Nothing

  tagRes <- liftIO getTags
  tags <- case tagRes of
    Left _ -> return []
    Right tags -> return tags

  errW <- case err of
    Nothing -> return mempty
    Just (errMsg, errDesc) -> return $ errorWidget errMsg errDesc

  defaultLayout $ errW <> $(whamletFile "templates/tags.hamlet")
