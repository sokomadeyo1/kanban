{-# LANGUAGE OverloadedStrings #-}

module Usecase.NewTag (newTag) where

import qualified Persistence.Sqlite as Persistence
import qualified Data.Text as T

newTag :: T.Text -> IO (Either T.Text ())
newTag tagName = do
  let t = T.strip tagName
  if (T.length t == 0)
    then return $ Left "Error: empty tag name"
    else Persistence.newTag tagName
