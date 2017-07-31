{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.CheckEligibility
  ( checkEligibilityEligible
  , CheckEligibility()
  ) where

import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Text.Lazy    (Text)
import           GHC.Generics

data CheckEligibility = CheckEligibility
  { _checkEligibilityResponseHost              :: Text
  , _checkEligibilityResponseId                :: Text
  , _checkEligibilityEligibilityReceipt        :: EligibilityReceipt
  , _checkEligibilityTermsAndConditionsAssetId :: Text
  , _checkEligibilityApplicableCardInfo        :: ApplicableCardInfo
  } deriving (Generic, Show)


data ApplicableCardInfo = ApplicableCardInfo
  { _applicableCardInfoIsSecurityCodeApplicable :: Bool }
  deriving (Generic, Show)


data EligibilityReceipt = EligibilityReceipt
  { _eligibilityReceiptValue           :: Text
  , _eligibilityReceiptValidForMinutes :: Int
  } deriving (Generic, Show)


checkEligibilityEligible :: CheckEligibility
checkEligibilityEligible = CheckEligibility
  { _checkEligibilityResponseHost = "site1.mastercard.com"
  , _checkEligibilityResponseId = "123456"
  , _checkEligibilityEligibilityReceipt = EligibilityReceipt
    { _eligibilityReceiptValue = "f9f027e5-629d-11e3-949a-0800200c9a66"
    , _eligibilityReceiptValidForMinutes = 60
    }
  , _checkEligibilityTermsAndConditionsAssetId = "a9f027e5-629d-11e3-949a-0800200c9a66"
  , _checkEligibilityApplicableCardInfo = ApplicableCardInfo { _applicableCardInfoIsSecurityCodeApplicable = False }
  }

deriveJSON (makeFieldsOption "_checkEligibility") ''CheckEligibility
deriveJSON (makeFieldsOption "_applicableCardInfo") ''ApplicableCardInfo
deriveJSON (makeFieldsOption "_eligibilityReceipt") ''EligibilityReceipt
