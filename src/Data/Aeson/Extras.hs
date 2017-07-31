module Data.Aeson.Extras where

import           Data.Aeson.TH
import           Data.Char     (toLower)
import           Data.List     (stripPrefix)


makeFieldsOption :: String -> Options
makeFieldsOption prefix = defaultOptions { fieldLabelModifier = fieldsTransformation prefix }


fieldsTransformation :: String -> String -> String
fieldsTransformation prefix name = maybe name (stripLastUnderscore . lowerFirst) $ stripPrefix prefix name
  where
    lowerFirst []     = []
    lowerFirst (x:xs) = toLower x : xs

    stripLastUnderscore []     = []
    stripLastUnderscore ['_']  = []
    stripLastUnderscore (x:xs) = x:stripLastUnderscore xs
