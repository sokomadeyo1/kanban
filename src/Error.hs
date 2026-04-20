{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Error (errorWidget) where

import qualified Data.Text as T
import Foundation
import Yesod

errorWidget :: T.Text -> T.Text -> Widget
errorWidget errMsg errDesc = $(whamletFile "templates/alert.hamlet")
