{-# LANGUAGE OverloadedStrings #-}
module Server.MDES
  ( mkConfig
  , runServer
  ) where

import           Control.Concurrent       (newMVar)
import           Control.Lens
import           Data.Default.Class       (def)
import qualified Data.Map                 as Map
import           Data.String              (fromString)
import           Network.Wai.Handler.Warp (setHost, setPort)
import           Server.MDES.Config
import           Server.MDES.WebApp
import           System.Log.Logger
import           Web.Scotty


runServer :: Config -> IO ()
runServer config = do
  updateGlobalLogger "default" (setLevel INFO)
  let opts = def { verbose = 1
                 , settings = setPort (config ^. serverPort) $
                              setHost (fromString (config ^. serverHost)) $
                              settings def
                 }
  db <- newMVar Map.empty
  statusDB <- newMVar Map.empty
  scottyOpts opts (webApp (config ^. serverPrivateKey) db statusDB)
