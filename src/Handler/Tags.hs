{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Tags (getTagsR) where

import Foundation
import Usecase.GetTags
import Util.PrettyPrint
import Yesod

getTagsR :: Handler Html
getTagsR = defaultLayout $ do
  result <- liftIO getTags
  case result of
    Left _ -> [whamlet||]
    Right tags -> $(whamletFile "templates/tags.hamlet")
