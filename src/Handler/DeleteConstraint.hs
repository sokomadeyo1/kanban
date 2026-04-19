module Handler.DeleteConstraint (postDeleteConstraintR) where

import Usecase.AllowMove
import qualified Data.Text as T
import Foundation
import Yesod

postDeleteConstraintR :: T.Text -> T.Text -> Handler ()
postDeleteConstraintR from to = do
  _ <- liftIO $ allowMove from to
  redirect ColumnsR
