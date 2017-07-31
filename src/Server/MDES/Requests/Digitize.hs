{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Server.MDES.Requests.Digitize
  ( Digitize()
  , responseHost
  , paymentAppInstanceId
  )
where

import           Control.Lens.TH
import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Text.Lazy    (Text)
import           GHC.Generics


data Digitize = Digitize
  { _digitizeResponseHost         :: Text
  , _digitizePaymentAppInstanceId :: Text
  } deriving (Generic, Show)


makeFields ''Digitize

deriveFromJSON (makeFieldsOption "_digitize") ''Digitize

