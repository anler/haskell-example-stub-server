{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.Common
  ( common,
    Status(ACTIVE, SUSPENDED, DELETED)
  ) where

import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text         (Text)
import           GHC.Generics


data Status = ACTIVE
            | SUSPENDED
            | DELETED
            deriving (Generic, Show, FromJSON, ToJSON)

data Common = Common
  { _commonResponseHost :: Text
  , _commonResponseId   :: Int
  } deriving (Generic, Show)


common :: Common
common = Common { _commonResponseHost = "site1.your-server.com"
                , _commonResponseId = 1234
                }


deriveJSON (makeFieldsOption "_common") ''Common
