{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Ignoring some warnings for Template Haskell reasons
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import qualified Data.Text as T
import Text.Hamlet (hamletFile)
import Yesod
import Text.Lucius

data App = App
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")
instance Yesod App where
  defaultLayout widget = do
    let navbarItems =
          [ ("Board" :: T.Text, BoardR)
          , ("Columns", ColumnsR)
          , ("Tags", TagsR)
          ]
    pc <- widgetToPageContent $ (toWidget $(luciusFile "templates/style.lucius")) <> widget
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

-- Required for using forms
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage
