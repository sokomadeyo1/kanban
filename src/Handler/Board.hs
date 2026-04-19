{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Board (getBoardR) where

import Domain.Entry
import Domain.Tag
import Foundation
import Usecase.GetEntries
import Util.PrettyPrint
import Yesod

getBoardR :: Handler Html
getBoardR = defaultLayout $ do
  board <- liftIO getEntries
  case board of
    Left _ -> [whamlet||]
    Right entries -> do
      let getid = (\(EntryID i) -> i) . entryID
      $(whamletFile "templates/board.hamlet")
