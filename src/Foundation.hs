{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Ignoring some warnings for Template Haskell reasons
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Foundation where

import qualified Data.Text as T
import Text.Hamlet (hamletFile)
import Yesod

data App = App
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")
instance Yesod App where
  defaultLayout widget = do
    let navbarItems =
          [ ("Board" :: T.Text, BoardR)
          , ("Columns", ColumnsR)
          ]
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")
