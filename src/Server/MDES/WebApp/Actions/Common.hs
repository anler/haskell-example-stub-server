{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.Common
  ( commonA
  ) where

import           Server.MDES.Responses.Common (common)
import           Web.Scotty


commonA :: ActionM ()
commonA = json common
