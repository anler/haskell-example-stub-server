{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.Digitize
  ( digitizeA
  ) where

import           Control.Concurrent               (readMVar)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (ask)
import           Control.Monad.Trans.Class        (lift)
import qualified Data.Map.Strict                  as Map
import           Server.MDES.Requests.Digitize
import qualified Server.MDES.Responses.Digitize   as Response
import           Server.MDES.WebApp.Actions.Types

import           Web.Scotty


digitizeA :: ActionM () -> ActionD ()
digitizeA cc = do
  (_, db) <- ask
  entries <- liftIO $ readMVar db
  dat <- lift $ jsonData
  let instanceId = (dat :: Digitize) ^. paymentAppInstanceId
  case Map.lookup instanceId entries of
    Just "5555555555554444" -> lift (json Response.digitizeStepUpMsg)
    _                       -> lift cc >> lift (json Response.digitizeApproved)
