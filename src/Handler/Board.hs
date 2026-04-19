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
  board <- liftIO getEntries
  case board of
    Left _ -> defaultLayout [whamlet||]
    Right entries -> do
      defaultLayout $(whamletFile "templates/board.hamlet")

postBoardR :: Handler Html
postBoardR = do
  ((formRes, widget), enctype) <- runFormPost postForm
  case formRes of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess q -> do
      _ <- liftIO $ newEntry (fst q) (snd q)
      return ()
  res <- liftIO getEntries
  case res of
    Left _ -> defaultLayout [whamlet||]
    Right entries ->
      defaultLayout $(whamletFile "templates/board.hamlet")
