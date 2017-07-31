{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.NotifyTokenUpdated
  ( notificationData
  , Notification()
  ) where

import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Text         (Text)
import           GHC.Generics


data Notification = Notification
  { _notificationResponseHost     :: Text
  , _notificationRequestId        :: Text
  , _notificationEncryptedPayload :: Map Text Text
  } deriving (Generic, Show)


deriveJSON (makeFieldsOption "_notification") ''Notification

notificationData :: Notification
notificationData = Notification
  { _notificationResponseHost = "site1.mastercard.com"
  , _notificationRequestId = "123456"
  , _notificationEncryptedPayload = Map.fromList
    [ ("encryptedData", "4545433044323232363739304532433610DE1D1461475BEB6D815F31764DDC20298BD779FBE37EE5AB3CBDA9F9825E1DDE321469537FE461E824AA55BA67BF6A")
    , ("publicKeyFingerprint", "4c4ead5927f0df8117f178eea9308daa58e27c2b")
    , ("encryptedKey", "A1B2C3D4E5F6112233445566")
    , ("oaepHashingAlgorithm", "SHA512")
    ]
  }
