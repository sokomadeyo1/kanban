module Handler (Handler, handle) where

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
handle GetEntries = getEntries
handle (NewEntry title desc) = newEntry title desc
handle (MoveEntry entryID colName) = moveEntry entryID colName
handle (NewColumn colName) = newColumnt colName
