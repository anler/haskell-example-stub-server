{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Server.MDES.Requests.CheckEligibility
  ( CheckEligibility()
  , CardInfo()
  , CardInfoData()
  , publicKeyFingerprint
  , encryptedKey
  , encryptedData
  , iv
  , oaepHashingAlgorithm
  , paymentAppInstanceId
  , cardInfo
  , accountNumber
  ) where

import           Control.Lens.TH
import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Text.Lazy    (Text)
import           GHC.Generics


data CheckEligibility = CheckEligibility
  { _checkEligibilityPaymentAppInstanceId :: Text
  , _checkEligibilityCardInfo             :: CardInfo
  } deriving (Generic, Show)


data CardInfo = CardInfo
  { _cardInfoPublicKeyFingerprint :: Text
  , _cardInfoEncryptedKey         :: Text
  , _cardInfoEncryptedData        :: Text
  , _cardInfoOaepHashingAlgorithm :: Text
  , _cardInfoIv                   :: Maybe Text
  } deriving (Generic, Show)


data CardInfoData = CardInfoData
  { _cardInfoDataAccountNumber :: Text
  } deriving (Generic, Show)


makeFields ''CheckEligibility
makeFields ''CardInfo
makeFields ''CardInfoData

deriveFromJSON (makeFieldsOption "_checkEligibility") ''CheckEligibility
deriveFromJSON (makeFieldsOption "_cardInfo") ''CardInfo
deriveFromJSON (makeFieldsOption "_cardInfoData") ''CardInfoData
