module Handler.DeleteTag (postDeleteTagR) where

import Foundation
import Usecase.DeleteTag
import Yesod
import qualified Data.Text as T

postDeleteTagR :: T.Text -> Handler Html
postDeleteTagR tagname = do
  _ <- liftIO $ deleteTag tagname
  redirect TagsR
