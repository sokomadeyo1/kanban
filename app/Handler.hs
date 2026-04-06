{-# LANGUAGE OverloadedStrings #-}

module Handler (Handler (..), handle) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Usecase.GetEntries (getEntries)
import Usecase.MoveEntry (moveEntry)
import Usecase.NewColumn (newColumn)
import Usecase.NewEntry (newEntry)
import PrettyPrint

data Handler
  = GetEntries
  | NewEntry T.Text T.Text
  | MoveEntry Int T.Text
  | NewColumn T.Text

handle :: Handler -> IO ()
handle GetEntries = do
  result <- getEntries
  case result of
    Left e -> TIO.putStrLn e
    Right r -> TIO.putStr $ pretty r
handle (NewEntry title desc) = do
  result <- newEntry title desc
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new entry:", title]
handle (MoveEntry entryID colName) = do
  result <- moveEntry entryID colName
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Moved entry to the", colName, "column"]
handle (NewColumn colName) = do
  result <- newColumn colName
  case result of
    Left e -> TIO.putStrLn e
    Right _ -> TIO.putStrLn $ T.unwords ["Created a new column:", colName]
