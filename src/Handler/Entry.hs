{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Entry (getEntryR, postEntryR) where

import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.GetOneEntry
import Usecase.RenameEntry
import Usecase.EditEntry
import Util.PrettyPrint
import Util.Cast (maybeToMonoid)
import Yesod

getid :: Entry -> Int
getid = (\(EntryID i) -> i) . entryID

entryFormGen :: Entry -> Html -> MForm Handler (FormResult Entry, Widget)
entryFormGen entry =
  let
    id_ = entryID entry
    col_ = entryColName entry
    entryGen = (\title desc -> Entry id_ title (maybeToMonoid desc) col_)
   in
    renderDivs $
      entryGen
        <$> areq textField "Title"       (Just $        entryTitle entry)
        <*> aopt textField "Description" (Just $ Just $ entryDesc  entry)

getEntryR :: Int -> Handler Html
getEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry, tags) -> do
      let formGen = entryFormGen entry
      ((_, widget), enctype) <- runFormPost formGen
      defaultLayout $(whamletFile "templates/entry.hamlet")

postEntryR :: Int -> Handler Html
postEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry_, tags) -> do
      let formGen = entryFormGen entry_
      ((formres, widget), enctype) <- runFormPost formGen
      case formres of
        FormMissing -> defaultLayout [whamlet||]
        FormFailure _ -> defaultLayout [whamlet||]
        FormSuccess q -> do
          _ <- liftIO $ renameEntry (EntryID i) (entryTitle q)
          _ <- liftIO $ editEntry (EntryID i) (entryDesc q)
          let entry = Entry { entryID = entryID entry_ , entryTitle = entryTitle q , entryDesc = entryDesc q , entryColName = entryColName entry_ }
          defaultLayout $(whamletFile "templates/entry.hamlet")
