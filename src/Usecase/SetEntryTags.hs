module Usecase.SetEntryTags (setEntryTags) where

import qualified Persistence.Sqlite as Persistence
import qualified Data.Text as T
import Domain.Entry
import Usecase.TagEntry
import Data.Either (lefts)

setEntryTags :: EntryID -> [T.Text] -> IO (Either T.Text ())
setEntryTags entryid tagnames = do
  gettags <- Persistence.getEntriesTags entryid
  case gettags of
    Left e -> return $ Left e
    Right entrytags -> do
      del <- if (entrytags /= []) 
        then Persistence.removeEntryTags entryid
        else return $ Right ()
      case del of
        Left e -> return $ Left e
        Right _ -> do
          res <- sequence $ map (tagEntry entryid) tagnames
          case lefts res of
            [] -> return $ Right ()
            errs -> return $ Left $ T.unlines errs
