{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.MDES.Requests.Suspend(
  SuspendRequest(..),
  Cause(..),
  ReasonCode(..),
  paymentAppInstanceId,
  tokenUniqueReferences,
  causedBy,
  reason,
  reasonCode
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Extras (makeFieldsOption)

data SuspendRequest = SuspendRequest {
  _suspendRequestPaymentAppInstanceId :: Text,
  _suspendRequestTokenUniqueReferences :: [Text],
  _suspendRequestCausedBy :: Cause,
  _suspendRequestReason :: Text,
  _suspendRequestReasonCode :: ReasonCode
  } deriving (Generic, Show)

data Cause = CARDHOLDER
           | PAYMENT_APP_PROVIDER
           | TOKEN_REQUESTOR
           | MOBILE_PIN_LOCKED
           | ISSUER
           deriving (Generic, Show, ToJSON)

data ReasonCode = DEVICE_LOST
                | DEVICE_STOLEN
                | SUSPECTED_FRAUD
                | OTHER
                deriving (Generic, Show, ToJSON)

makeFields ''SuspendRequest


deriveFromJSON (makeFieldsOption "_suspendRequest") ''SuspendRequest
deriveFromJSON defaultOptions ''Cause
deriveFromJSON defaultOptions ''ReasonCode
