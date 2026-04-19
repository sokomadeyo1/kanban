{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ViewPatterns #-}

module Application (appMain) where

import Foundation

-- Handlers
import Handler.Board
import Handler.Column
import Handler.Columns
import Handler.Constr
import Handler.Entry
import Handler.Tags

import Handler.DeleteEntry
import Handler.DeleteColumn
import Handler.DeleteConstraint

import Yesod

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
  warp 3000 App
