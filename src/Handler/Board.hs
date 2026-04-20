{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Board (getBoardR, postBoardR) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.GetEntries
import Usecase.NewEntry
import Util.Cast (getid, maybeToMonoid)
import Util.PrettyPrint
import Yesod
import Error

postForm :: Html -> MForm Handler (FormResult (T.Text, T.Text), Widget)
postForm =
  let
    parse = (\title desc -> (title, maybeToMonoid desc))
   in
    renderDivs $
      parse
        <$> areq textField "Title"       Nothing
        <*> aopt textField "Description" Nothing

getBoardR :: Handler Html
getBoardR = do
  ((_, widget), enctype) <- runFormPost postForm
  res <- liftIO getEntries
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries
  defaultLayout $(whamletFile "templates/board.hamlet")

postBoardR :: Handler Html
postBoardR = do
  ((formRes, widget), enctype) <- runFormPost postForm
  err <- case formRes of
    FormMissing -> return $ Just ("Error", "Form missing")
    FormFailure e -> return $ Just ("Error", T.append "Form failure: " $ T.show e)
    FormSuccess q -> do
      res <- liftIO $ newEntry (fst q) (snd q)
      case res of
        Left e -> return $ Just ("Error", e)
        Right _ -> return Nothing
  res <- liftIO getEntries
  entries <- case res of
    Left _ -> return []
    Right entries -> return entries
  errW <- case err of
    Nothing -> return mempty
    Just (errMsg, errDesc) -> return $ errorWidget errMsg errDesc
  defaultLayout $ errW <> $(whamletFile "templates/board.hamlet")
