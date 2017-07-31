{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.WebApp.Assets
  ( assets
  ) where

import           Data.Aeson.Extras      (makeFieldsOption)
import           Data.Aeson.TH
import qualified Data.ByteString.Base64 as BS64
import           Data.FileEmbed         (embedFile)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           GHC.Generics

assets :: Map Text Asset
assets = Map.fromList
  [
    (
      "a9f027e5-629d-11e3-949a-0800200c9a66"
    , Asset
      { _assetMediaContents =
        [ AssetMimeText
          { _assetMimeType = "text/plain"
          , _assetMimeData = ((T.decodeUtf8 . BS64.encode) $(embedFile "assets/terms-and-conditions.txt"))
          }
        ]
      }
    )
  ,
    (
      "739d27e5-629d-11e3-949a-0800200c9a66"
    , Asset
      [ AssetMimeImage { _assetMimeType = "image/png"
                       , _assetMimeData = ((T.decodeUtf8 . BS64.encode) $(embedFile "assets/combined.png"))
                       , _assetMimeWidth = 310
                       , _assetMimeHeight = 310
                       }
      ]
    )
  ]


data AssetMime =
  AssetMimeText { _assetMimeType :: Text, _assetMimeData :: Text }
  | AssetMimeImage { _assetMimeType :: Text, _assetMimeData :: Text, _assetMimeWidth :: Int, _assetMimeHeight :: Int }
  deriving (Generic, Show)


data Asset = Asset { _assetMediaContents :: [AssetMime] }
  deriving (Generic, Show)


deriveJSON (makeFieldsOption "_assetMime"){ sumEncoding = UntaggedValue } ''AssetMime
deriveJSON (makeFieldsOption "_asset") ''Asset
