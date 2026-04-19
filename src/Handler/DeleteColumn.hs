module Handler.DeleteColumn (postDeleteColumnR) where

import Foundation
import Usecase.DeleteColumn
import Yesod
import qualified Data.Text as T

postDeleteColumnR :: T.Text -> Handler Html
postDeleteColumnR colname = do
  _ <- liftIO $ deleteColumn colname
  redirect ColumnsR
