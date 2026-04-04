module Handler (Handler(..), handle) where

import qualified Data.Text as T
import Usecase.GetEntries (getEntries)
import Usecase.MoveEntry (moveEntry)
import Usecase.NewColumn (newColumn)
import Usecase.NewEntry (newEntry)

data Handler
  = GetEntries
  | NewEntry T.Text T.Text
  | MoveEntry Int T.Text
  | NewColumn T.Text

handle :: Handler -> IO ()
handle GetEntries = do
  result <- getEntries
  case result of
    Left e -> print e
    Right r -> print r
handle (NewEntry title desc) = do
  result <- newEntry title desc
  case result of
    Left e -> print e
    Right r -> print r
handle (MoveEntry entryID colName) = do
  result <- moveEntry entryID colName
  case result of
    Left e -> print e
    Right r -> print r
handle (NewColumn colName) = do
  result <- newColumn colName
  case result of
    Left e -> print e
    Right r -> print r
