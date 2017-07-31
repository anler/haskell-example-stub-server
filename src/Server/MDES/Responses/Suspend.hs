{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.Suspend(
  SuspendResponse(..),
  Token(..),
  Error(..),
  tokens,
  status,
  errors,
  productConfig,
  suspendedBy,
  tdsRegistrationUrl,
  tokenInfo,
  tokenUniqueReference,
  reasonCode,
  description,
  errorCode,
  source
  ) where

import GHC.Generics
import Data.Text.Lazy
import Control.Lens.TH
import Data.Aeson.Extras (makeFieldsOption)
import Data.Aeson.TH
import Server.MDES.Responses.Common
import Server.MDES.Responses.Digitize
import Server.MDES.Requests.Suspend hiding (reasonCode)

data SuspendResponse = SuspendResponse {
  _suspendResponseTokens :: [Token]
  } deriving (Generic, Show)

data Token = Token {
  _tokenTokenUniqueReference :: Text,
  _tokenStatus :: Status,
  _tokenSuspendedBy :: Maybe Cause,
  _tokenProductConfig :: Maybe ProductConfig,
  _tokenTokenInfo :: Maybe TokenInfo,
  _tokenTdsRegistrationUrl :: Maybe Text,
  _tokenErrors :: Maybe [Error]
  } deriving (Generic, Show)

data Error = Error {
  _errorSource :: Text,
  _errorErrorCode :: Text,
  _errorDescription :: Text,
  _errorReasonCode :: ReasonCode
  } deriving (Generic, Show)

makeFields ''SuspendResponse
makeFields ''Token
makeFields ''Error

deriveToJSON (makeFieldsOption "_suspendResponse") ''SuspendResponse
deriveToJSON (makeFieldsOption "_token") ''Token
deriveToJSON (makeFieldsOption "_error") ''Error
