{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Entry (getEntryR, postEntryR) where

import qualified Data.Text as T
import Domain.Column
import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.EditEntry
import Usecase.GetColumns
import Usecase.GetOneEntry
import Usecase.MoveEntry
import Usecase.RenameEntry
import Util.Cast (getid, maybeToMonoid)
import Util.PrettyPrint
import Yesod
import Usecase.GetTags
import Usecase.SetEntryTags
import Util.Class (dummy)

entryFormGen ::
  Entry ->
  [T.Text] ->
  [T.Text] ->
  [T.Text] ->
  Html ->
  MForm Handler (FormResult (Entry, [T.Text]), Widget)
entryFormGen entry columns alltags tags =
  let
    id_ = entryID entry
    entryGen = (\title desc col tags_ -> (Entry id_ title (maybeToMonoid desc) col, (maybeToMonoid tags_)))
    columnlist = fmap (\x -> (x, x)) columns
    taglist = fmap (\x -> (x, x)) alltags
   in
    renderDivs $
      entryGen
        <$> areq textField "Title" (Just $ entryTitle entry)
        <*> aopt textField "Description" (Just $ Just $ entryDesc entry)
        <*> areq (selectFieldList columnlist) "Column" (Just $ entryColName entry)
        <*> aopt (checkboxesFieldList taglist) "Tags" (Just $ Just tags)

getEntryR :: Int -> Handler Html
getEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry, tags) -> do
      getcols <- liftIO getColumns
      cols <- case getcols of
        Left _ -> return []
        Right cols -> return cols

      gettags <- liftIO getTags
      alltags <- case gettags of
        Left _ -> return []
        Right alltags -> return alltags

      let colnames = map columnTitle cols
      let tagnames = map tagName tags
      let alltagnames = map tagName alltags
      let formGen = entryFormGen entry colnames alltagnames tagnames
      ((_, widget), enctype) <- runFormPost formGen
      defaultLayout $(whamletFile "templates/entry.hamlet")

postEntryR :: Int -> Handler Html
postEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry_, tags_) -> do
      getcols <- liftIO getColumns
      cols <- case getcols of
        Left _ -> return []
        Right cols -> return cols

      gettags <- liftIO getTags
      alltags <- case gettags of
        Left _ -> return []
        Right alltags -> return alltags

      let colnames = map columnTitle cols
      let tagnames = map tagName tags_
      let alltagnames = map tagName alltags
      let formGen = entryFormGen entry_ colnames alltagnames tagnames
      ((formRes, widget), enctype) <- runFormPost formGen
      case formRes of
        FormMissing -> defaultLayout [whamlet||]
        FormFailure e -> defaultLayout [whamlet|#{show e}|]
        FormSuccess (e, ts) -> do
          _ <- liftIO $ renameEntry (EntryID i) (entryTitle e)
          _ <- liftIO $ editEntry (EntryID i) (entryDesc e)
          _ <- liftIO $ moveEntry (EntryID i) (entryColName e)
          _ <- liftIO $ setEntryTags (EntryID i) ts
          let entry = Entry (entryID entry_) (entryTitle e) (entryDesc e) (entryColName entry_)
          let tags = map dummy ts
          defaultLayout $(whamletFile "templates/entry.hamlet")
