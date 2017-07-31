{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp
  ( webApp
  ) where

import           Control.Concurrent                            (MVar)
import           Control.Monad.Reader                          (runReaderT)
import           Crypto.PubKey.RSA                             (PrivateKey)
import           Data.Map                                      (Map)
import           Data.Text.Lazy                                (Text)
import           Data.Time                                     (getZonedTime)
import qualified Network.HTTP.Types.Status                     as Status
import           Network.Wai                                   (Middleware)
import           Network.Wai.Middleware.RequestLogger          (logStdoutDev)
import           Server.MDES.WebApp.Actions.Asset
import           Server.MDES.WebApp.Actions.CheckEligibility
import           Server.MDES.WebApp.Actions.Common
import           Server.MDES.WebApp.Actions.Digitize
import           Server.MDES.WebApp.Actions.NotifyTokenUpdated
import           Server.MDES.WebApp.Actions.Provision
import           Server.MDES.WebApp.Actions.Suspend
import qualified Server.MDES.WebApp.Urls                       as Urls
import           Web.Scotty


webApp :: PrivateKey -> MVar (Map Text Text) -> MVar (Map Text Status) -> ScottyM ()
webApp serverKey db statusDB = do
  middleware $ logStdoutDev . printCurrentTime

  let notAllowed = status Status.methodNotAllowed405
      env = (serverKey, db)

  get (literal Urls.checkEligibility) $ notAllowed
  post (literal Urls.checkEligibility) $ (runReaderT checkEligibilityA env) `rescue` errorHandler

  get (literal Urls.digitize) $ notAllowed
  post (literal Urls.digitize) $ (runReaderT (digitizeA provisionA) env) `rescue` errorHandler

  get (literal Urls.requestActivationCode) $ notAllowed
  post (literal Urls.requestActivationCode) $ commonA

  -- get (literal Urls.activate) $ notAllowed
  -- post (literal Urls.activate) $ (commonA >> notifyTokenUpdatedA)

  post (literal Urls.suspend) $ (runReaderT suspendA statusDB)
  post (literal Urls.unsuspend) $ (runReaderT unsuspendA statusDB)
  post (literal Urls.delete) $ (runReaderT deleteA statusDB)

  get (capture (Urls.asset ++ "/:id")) $ assetA


printCurrentTime :: Middleware
printCurrentTime app req res = do
  time <- getZonedTime
  putStrLn $ "\n" ++ show time
  app req res


errorHandler :: Text -> ActionM ()
errorHandler msg = do
  status Status.badRequest400
  text msg
