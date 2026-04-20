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
  case formRes of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess q -> do
      _ <- liftIO $ newTag q
      return ()

  tagRes <- liftIO getTags
  tags <- case tagRes of
    Left _ -> return []
    Right tags -> return tags

  defaultLayout $(whamletFile "templates/tags.hamlet")
