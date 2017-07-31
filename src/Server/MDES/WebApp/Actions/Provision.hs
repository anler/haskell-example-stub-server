{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.Provision
  ( provisionA
  ) where

import           Server.MDES.Responses.Provision
import           Server.MDES.WebApp.Actions
import           Web.Scotty


provisionA :: ActionM ()
provisionA = asyncResponse "/provision" provisionData
