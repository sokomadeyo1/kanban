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

tagFormGen :: T.Text -> Html -> MForm Handler (FormResult T.Text, Widget)
tagFormGen colname = renderDivs $ areq textField "Tag name" (Just $ colname)

getTagR :: T.Text -> Handler Html
getTagR tagname = do
  res <- liftIO $ getEntriesByTag tagname
  let formGen = tagFormGen tagname
  ((_, widget), enctype) <- runFormPost formGen
  case res of
    Left _ -> do
      let entries = [] :: [(Entry, [Tag])]
      defaultLayout $(whamletFile "templates/tag.hamlet")
    Right entries -> do
      defaultLayout $(whamletFile "templates/tag.hamlet")

postTagR :: T.Text -> Handler Html
postTagR tagname_ = do
  res <- liftIO $ getEntriesByTag tagname_
  case res of
    Left _ -> defaultLayout $ [whamlet||]
    Right entries -> do
      let formGen = tagFormGen tagname_
      ((formRes, widget), enctype) <- runFormPost formGen
      case formRes of
        FormMissing -> defaultLayout [whamlet||]
        FormFailure _ -> defaultLayout [whamlet||]
        FormSuccess newname -> do
          renameRes <- liftIO $ renameTag tagname_ newname
          case renameRes of
            Left _ -> defaultLayout [whamlet||]
            Right _ -> do
              let tagname = newname
              defaultLayout $(whamletFile "templates/tag.hamlet")
