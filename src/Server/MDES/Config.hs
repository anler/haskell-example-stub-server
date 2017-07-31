{-# LANGUAGE TemplateHaskell #-}
module Server.MDES.Config
  ( mkConfig
  , serverHost
  , serverPort
  , serverPrivateKey
  , Config()
  ) where

import           Control.Exception
import qualified Control.Arrow as Arrow
import           Control.Monad.Except
import           Crypto.PubKey.RSA    (PrivateKey)
import           Data.Attoparsec.Text (decimal, parseOnly)
import qualified Data.Text            as Text
import           Data.X509            (PrivKey (..))
import           Data.X509.File       (readKeyFile)
import Control.Lens.TH


data Config = Config
  { _serverHost       :: String
  , _serverPort       :: Int
  , _serverPrivateKey :: PrivateKey
  } deriving Show

makeLenses ''Config


mkConfig :: String -> String -> FilePath -> IO (Either String Config)
mkConfig host port keyPath =
  runExceptT $
  Config <$>
  ExceptT (pure (validateHost host)) <*>
  ExceptT (pure (validatePort port)) <*>
  validateKeyPath keyPath


validateHost :: String -> Either String String
validateHost = pure


validatePort :: String -> Either String Int
validatePort = parseOnly decimal . Text.pack


validateKeyPath :: String -> ExceptT String IO PrivateKey
validateKeyPath path =
  asPrivateKey =<<
  ExceptT (Arrow.left (displayException :: SomeException -> String) <$> try (head <$> readKeyFile path))

  where

    asPrivateKey :: PrivKey -> ExceptT String IO PrivateKey
    asPrivateKey (PrivKeyRSA key) = return key
    asPrivateKey _ = throwError "Only RSA keys are supported."
