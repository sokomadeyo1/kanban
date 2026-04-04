module Main (main) where

import qualified Data.Text as T
import Usecase.GetEntries
import Usecase.NewEntry

main :: IO ()
main = do
  title_ <- getLine
  desc_ <- getLine
  let title = T.pack title_
  let desc = T.pack desc_
  newEntry title desc
  entries <- getEntries
  print entries
