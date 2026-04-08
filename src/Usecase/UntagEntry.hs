{-# LANGUAGE OverloadedStrings #-}

module Usecase.UntagEntry (untagEntry) where

import qualified Data.Text as T
import Domain.Entry
import Domain.Tag
import qualified Persistence.Sqlite as Persistence
import Util.Class (dummy)
import Util.PrettyPrint

untagEntry :: EntryID -> T.Text -> IO (Either T.Text ())
untagEntry entryid tagname = do
  entry <- Persistence.getOneEntry entryid
  case entry of
    Left err -> return $ Left err
    Right _ -> do
      result <- Persistence.getOneTag tagname
      case result of
        Left err -> return $ Left err
        Right (Tag tagid _) -> do
          checktags <- Persistence.getEntriesTags entryid
          case checktags of
            Right tags -> do
              if elem (Tag tagid tagname) tags
                then Persistence.untagEntry entryid tagid
                else return $ Left $ T.unwords
                  [ pretty entryid
                  , "is not tagged with"
                  , pretty (dummy tagname :: Tag)
                  ]
            _ -> return $ Left $ T.unwords 
              [ pretty entryid
              , "is not tagged with"
              , pretty (dummy tagname :: Tag)
              ]

-- This readability T_T
