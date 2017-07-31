{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.Asset
  ( assetA
  ) where

import qualified Data.Map                  as Map
import           Data.Maybe                (maybe)
import qualified Network.HTTP.Types.Status as Status
import           Server.MDES.WebApp.Assets (assets)
import           Web.Scotty


assetA :: ActionM ()
assetA = do
  assetId <- param "id"
  maybe (status Status.notFound404) json (Map.lookup assetId assets)
