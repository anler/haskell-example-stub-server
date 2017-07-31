{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Server.MDES.Requests.Common
  ( Common()
  , responseHost
  ) where

import           Control.Lens.TH
import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Text.Lazy    (Text)
import           GHC.Generics


data Common = Common
  { _commonResponseHost :: Text
  } deriving (Generic, Show)


makeFields ''Common


deriveFromJSON (makeFieldsOption "_common") ''Common
