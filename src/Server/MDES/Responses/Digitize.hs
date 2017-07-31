{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.Digitize
  ( Digitize()
  , digitizeApproved
  , digitizeStepUpMsg
  , ProductConfig()
  , TokenInfo()
  )
where

import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Text.Lazy    (Text)
import           GHC.Generics


data Digitize = Digitize
  { _digitizeResponseHost          :: Text
  , _digitizeResponseId            :: Text
  , _digitizeDecision              :: Text
  , _digitizeAuthenticationMethods :: [AuthenticationMethod]
  , _digitizeTokenUniqueReference  :: Text
  , _digitizePanUniqueReference    :: Text
  , _digitizeProductConfig         :: ProductConfig
  , _digitizeTokenInfo             :: TokenInfo
  , _digitizeTdsRegistrationUrl    :: Text
  } deriving (Generic, Show)


data AuthenticationMethod = AuthenticationMethod
  { _authenticationMethodId    :: Int
  , _authenticationMethodType_ :: Text
  , _authenticationMethodValue :: Text
  } deriving (Generic, Show)


data ProductConfig = ProductConfig
  { _productConfigBrandLogoAssetId              :: Text
  , _productConfigIssuerLogoAssetId             :: Text
  , _productConfigIconAssetId                   :: Text
  , _productConfigIsCoBranded                   :: Text
  , _productConfigCoBrandName                   :: Text
  , _productConfigCoBrandLogoAssetId            :: Text
  , _productConfigCardBackgroundCombinedAssetId :: Text
  , _productConfigForegroundColor               :: Text
  , _productConfigIssuerName                    :: Text
  , _productConfigShortDescription              :: Text
  , _productConfigLongDescription               :: Text
  , _productConfigCustomerServiceUrl            :: Text
  , _productConfigIssuerMobileApp               :: Map Text (Map Text Text)
  , _productConfigTermsAndConditionsUrl         :: Text
  , _productConfigPrivacyPolicyUrl              :: Text
  , _productConfigIssuerProductConfigCode       :: Text
  } deriving (Generic, Show)


data TokenInfo = TokenInfo
  { _tokenInfoTokenPanSuffix   :: Text
  , _tokenInfoAccountPanSuffix :: Text
  , _tokenInfoTokenExpiry      :: Text
  , _tokenInfoDsrpCapable      :: Bool
  } deriving (Generic, Show)


deriveJSON (makeFieldsOption "_digitize") ''Digitize
deriveJSON (makeFieldsOption "_authenticationMethod") ''AuthenticationMethod
deriveJSON (makeFieldsOption "_productConfig") ''ProductConfig
deriveJSON (makeFieldsOption "_tokenInfo") ''TokenInfo


digitizeApproved :: Digitize
digitizeApproved = Digitize
  { _digitizeResponseHost = "site1.mastercard.com"
  , _digitizeResponseId = "123123"
  , _digitizeDecision = "APPROVED"
  , _digitizeAuthenticationMethods = mempty
  , _digitizeTokenUniqueReference = "DWSPMC000000000132d72d4fcb2f4136a0532d3093ff1a45"
  , _digitizePanUniqueReference = "FWSPMC000000000159f71f703d2141efaf04dd26803f922b"
  , _digitizeTdsRegistrationUrl = "tds.Mastercard.com"
  , _digitizeTokenInfo = TokenInfo
    { _tokenInfoTokenPanSuffix = "1234"
    , _tokenInfoAccountPanSuffix = "6789"
    , _tokenInfoTokenExpiry = "1018"
    , _tokenInfoDsrpCapable = True
    }
  , _digitizeProductConfig = ProductConfig
    { _productConfigBrandLogoAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIssuerLogoAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIconAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIsCoBranded = "true"
    , _productConfigCoBrandName = "Co brand partner"
    , _productConfigCoBrandLogoAssetId = "dbc55444-496a-4896-b41c-5d5e2dd431e2"
    , _productConfigCardBackgroundCombinedAssetId = "739d27e5-629d-11e3-949a-0800200c9a66"
    , _productConfigForegroundColor = "000000"
    , _productConfigIssuerName = "Issuing Bank"
    , _productConfigShortDescription = "Bank Rewards Mastercard"
    , _productConfigLongDescription = "Bank Rewards Mastercard with the super duper rewards program"
    , _productConfigCustomerServiceUrl = "https://bank.com/customerservice"
    , _productConfigTermsAndConditionsUrl = "https://bank.com/termsAndConditions"
    , _productConfigPrivacyPolicyUrl = "https://bank.com/privacy"
    , _productConfigIssuerProductConfigCode = "123456"
    , _productConfigIssuerMobileApp = Map.fromList
      [
        (
          "openIssuerMobileAppAndroidIntent"
        , Map.fromList
          [
            ("action", "com.mybank.bankingapp.action.OPEN_ISSUER_MOBILE_APP")
          , ("packageName", "com.mybank.bankingapp")
          , ("extraTextValue", "ew0KICAgICJwYXltZW50QXBwUHJvdmlkZXJJZCI6ICIxMjM0NTY3ODkiLA0KICAgICJwYXltZW50Q XBwSWQiOiAiV2FsbGV0QXBwMSIsDQogICAgInBheW1lbnRBcHBJbnN0YW5jZUlkIjogIjEyMzQ1Njc 4OSIsDQogICAgInRva2VuVW5pcXVlUmVmZXJlbmNlIjogIkRXU1BNQzAwMDAwMDAwMGZjYjJmNDEzN mIyZjQxMzZhMDUzMmQyZjQxMzZhMDUzMiINCn0=")
          ]
        )
      ]
    }
  }


digitizeStepUpMsg :: Digitize
digitizeStepUpMsg = Digitize
  { _digitizeResponseHost = "site1.mastercard.com"
  , _digitizeResponseId = "123123"
  , _digitizeDecision = "REQUIRE_ADDITIONAL_AUTHENTICATION"
  , _digitizeAuthenticationMethods =
      [ AuthenticationMethod
        { _authenticationMethodId = 12344
        , _authenticationMethodType_ = "TEXT_TO_CARDHOLDER_NUMBER"
        , _authenticationMethodValue = "12X-XXX-XX32"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12345
        , _authenticationMethodType_ = "EMAIL_TO_CARDHOLDER_ADDRESS"
        , _authenticationMethodValue = "cardh****@***com"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12346
        , _authenticationMethodType_ = "CARDHOLDER_TO_CALL_AUTOMATED_NUMBER"
        , _authenticationMethodValue = "1-800-BANK-NUMBER"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12347
        , _authenticationMethodType_ = "CARDHOLDER_TO_CALL_MANNED_NUMBER"
        , _authenticationMethodValue = "1-800-BANK-NUMBER"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12348
        , _authenticationMethodType_ = "CARDHOLDER_TO_VISIT_WEBSITE"
        , _authenticationMethodValue = "http://issuer.website.url"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12349
        , _authenticationMethodType_ = "CARDHOLDER_TO_USE_ISSUER_MOBILE_APP"
        , _authenticationMethodValue = "{\"activateWithIssuerMobileAppAndroidIntent\":\"{\"action\" : \"com.mybank.bankingapp.action.ACTIVATE_TOKEN\",\"packageName\" : \"com.mybank.bankingapp\",\"extraTextValue\" : \"ew0KICAgICJwYXltZW50QXBwUHJvdmlkZXJJZCI6ICIxMjM0NTY3ODkiLA0KICAgICJwYXltZW50QXBwSW5 zdGFuY2VJZCI6ICIxMjM0NTY3ODkiLA0KICAgICJ0b2tlblVuaXF1ZVJlZmVyZW5jZSI6ICJEV1NQTUMwMDAw MDAwMDBmY2IyZjQxMzZiMmY0MTM2YTA1MzJkMmY0MTM2YTA1MzIiLA0KICAgICJhY2NvdW50UGFuU3VmZml4I jogIjY3ODkiLA0KICAgICJhY2NvdW50RXhwaXJ5IjogIjEwMTgiDQp9\"}\"}"
        }
      , AuthenticationMethod
        { _authenticationMethodId = 12350
        , _authenticationMethodType_ = "ISSUER_TO_CALL_CARDHOLDER_NUMBER"
        , _authenticationMethodValue = "1-800-CARDHOLDER-NUMBER"
        }
      ]
  , _digitizeTokenUniqueReference = "DWSPMC000000000132d72d4fcb2f4136a0532d3093ff1a45"
  , _digitizePanUniqueReference = "FWSPMC000000000159f71f703d2141efaf04dd26803f922b"
  , _digitizeTdsRegistrationUrl = "tds.Mastercard.com"
  , _digitizeTokenInfo = TokenInfo
    { _tokenInfoTokenPanSuffix = "1234"
    , _tokenInfoAccountPanSuffix = "6789"
    , _tokenInfoTokenExpiry = "1018"
    , _tokenInfoDsrpCapable = True
    }
  , _digitizeProductConfig = ProductConfig
    { _productConfigBrandLogoAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIssuerLogoAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIconAssetId = "800200c9-629d-11e3-949a-0739d27e5a66"
    , _productConfigIsCoBranded = "true"
    , _productConfigCoBrandName = "Co brand partner"
    , _productConfigCoBrandLogoAssetId = "dbc55444-496a-4896-b41c-5d5e2dd431e2"
    , _productConfigCardBackgroundCombinedAssetId = "739d27e5-629d-11e3-949a-0800200c9a66"
    , _productConfigForegroundColor = "000000"
    , _productConfigIssuerName = "Issuing Bank"
    , _productConfigShortDescription = "Bank Rewards Mastercard"
    , _productConfigLongDescription = "Bank Rewards Mastercard with the super duper rewards program"
    , _productConfigCustomerServiceUrl = "https://bank.com/customerservice"
    , _productConfigTermsAndConditionsUrl = "https://bank.com/termsAndConditions"
    , _productConfigPrivacyPolicyUrl = "https://bank.com/privacy"
    , _productConfigIssuerProductConfigCode = "123456"
    , _productConfigIssuerMobileApp = Map.fromList
      [
        (
          "openIssuerMobileAppAndroidIntent"
        , Map.fromList
          [
            ("action", "com.mybank.bankingapp.action.OPEN_ISSUER_MOBILE_APP")
          , ("packageName", "com.mybank.bankingapp")
          , ("extraTextValue", "ew0KICAgICJwYXltZW50QXBwUHJvdmlkZXJJZCI6ICIxMjM0NTY3ODkiLA0KICAgICJwYXltZW50Q XBwSWQiOiAiV2FsbGV0QXBwMSIsDQogICAgInBheW1lbnRBcHBJbnN0YW5jZUlkIjogIjEyMzQ1Njc 4OSIsDQogICAgInRva2VuVW5pcXVlUmVmZXJlbmNlIjogIkRXU1BNQzAwMDAwMDAwMGZjYjJmNDEzN mIyZjQxMzZhMDUzMmQyZjQxMzZhMDUzMiINCn0=")
          ]
        )
      ]
    }
  }
