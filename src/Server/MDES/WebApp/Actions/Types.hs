module Server.MDES.WebApp.Actions.Types
  ( ActionD
  ) where

import           Control.Concurrent   (MVar)
import           Control.Monad.Reader
import           Crypto.PubKey.RSA
import           Data.Map.Strict      (Map)
import           Data.Text.Lazy       (Text)
import           Web.Scotty

type ActionD = ReaderT (PrivateKey, MVar (Map Text Text)) ActionM
