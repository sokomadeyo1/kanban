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

entryFormGen :: Entry -> [T.Text] -> Html -> MForm Handler (FormResult Entry, Widget)
entryFormGen entry columns =
  let
    id_ = entryID entry
    entryGen = (\title desc col -> Entry id_ title (maybeToMonoid desc) col)
    columnlist = fmap (\x -> (x, x)) columns
   in
    renderDivs $
      entryGen
        <$> areq textField "Title"       (Just $        entryTitle entry)
        <*> aopt textField "Description" (Just $ Just $ entryDesc  entry)
        <*> areq (selectFieldList columnlist) "Column" (Just $ entryColName entry)

getEntryR :: Int -> Handler Html
getEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry, tags) -> do
      getcols <- liftIO getColumns
      case getcols of
        Left _ -> defaultLayout [whamlet||]
        Right cols -> do
          let colnames = map columnTitle cols
          let formGen = entryFormGen entry colnames
          ((_, widget), enctype) <- runFormPost formGen
          defaultLayout $(whamletFile "templates/entry.hamlet")

postEntryR :: Int -> Handler Html
postEntryR i = do
  res <- liftIO $ getOneEntry $ EntryID i
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right (entry_, tags) -> do
      getcols <- liftIO getColumns
      case getcols of
        Left _ -> defaultLayout [whamlet||]
        Right cols -> do
          let colnames = map columnTitle cols
          let formGen = entryFormGen entry_ colnames
          ((formRes, widget), enctype) <- runFormPost formGen
          case formRes of
            FormMissing -> defaultLayout [whamlet||]
            FormFailure _ -> defaultLayout [whamlet||]
            FormSuccess q -> do
              _ <- liftIO $ renameEntry (EntryID i) (entryTitle q)
              _ <- liftIO $ editEntry (EntryID i) (entryDesc q)
              _ <- liftIO $ moveEntry (EntryID i) (entryColName q)
              let entry = Entry { entryID = entryID entry_ , entryTitle = entryTitle q , entryDesc = entryDesc q , entryColName = entryColName entry_ }
              defaultLayout $(whamletFile "templates/entry.hamlet")
