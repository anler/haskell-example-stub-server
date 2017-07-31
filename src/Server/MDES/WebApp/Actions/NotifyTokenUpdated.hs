{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.NotifyTokenUpdated
  ( notifyTokenUpdatedA
  ) where



import           Server.MDES.Responses.NotifyTokenUpdated
import           Server.MDES.WebApp.Actions
import           Web.Scotty


notifyTokenUpdatedA :: ActionM ()
notifyTokenUpdatedA = asyncResponse "/notifyTokenUpdated" notificationData
