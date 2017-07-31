{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.CheckEligibility
  ( checkEligibilityA
  ) where

import           Control.Concurrent                     (modifyMVar_)
import           Control.Lens
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Reader                   (ask)
import           Control.Monad.Trans.Class              (lift)
import           Crypto.Cipher.AES                      (AES128)
import           Crypto.Cipher.Types                    (BlockCipher (..),
                                                         Cipher (..), IV,
                                                         makeIV, nullIV)
import           Crypto.Data.Padding                    (Format (PKCS7), unpad)
import           Crypto.Error                           (CryptoFailable (..))
import           Crypto.Hash.Algorithms                 (SHA256 (..),
                                                         SHA512 (..))
import           Crypto.PubKey.RSA                      (PrivateKey)
import           Crypto.PubKey.RSA.OAEP
import qualified Data.Aeson                             as Json
import qualified Data.ByteString.Base16.Lazy            as BS
import           Data.ByteString.Lazy                   (ByteString)
import qualified Data.ByteString.Lazy                   as BS
import qualified Data.Map.Strict                        as Map
import           Data.Maybe                             (fromJust)
import           Data.Text.Lazy                         (Text)
import qualified Data.Text.Lazy                         as T
import qualified Data.Text.Lazy.Encoding                as T
import           Debug.Trace
import qualified Network.HTTP.Types.Status              as Status
import           Server.MDES.Requests.CheckEligibility
import           Server.MDES.Responses.CheckEligibility (checkEligibilityEligible)
import           Server.MDES.WebApp.Actions.Types
import           System.Log.Logger
import           Web.Scotty


checkEligibilityA :: ActionD ()
checkEligibilityA = do
  (privateKey, db) <- ask
  dat <- lift $ jsonData
  let instanceId = dat ^. paymentAppInstanceId
  case getCardData dat privateKey of
    Left err         -> do
      let msg = show err
      liftIO $ infoM "default" $ "Error recovering card data: " ++ msg
      lift $ status Status.badRequest400
      lift $ json $ T.pack msg

    Right cardData -> do
      let cardNumber = cardData ^. accountNumber
      liftIO $ infoM "default" $ "Recovered card data: " ++ show cardData
      liftIO $ modifyMVar_ db (return . Map.insertWith const instanceId cardNumber)
      lift $ json $ checkEligibilityEligible


getCardData :: CheckEligibility -> PrivateKey -> Either String CardInfoData
getCardData dat pk = do
  let inf = dat ^. cardInfo
  key <- recoverKey pk (inf ^. encryptedKey) (inf ^. oaepHashingAlgorithm)
  trace "✓ Recovered key" $ return ()
  v <- recoverIv (inf ^. iv)
  trace "✓ Recovered IV" $ return ()
  cardData <- recoverCardData key (inf ^. encryptedData) v
  trace "✓ Recovered Card Data" $ return ()
  return cardData


recoverKey :: PrivateKey -> Text -> Text -> Either String AES128
recoverKey pk encKey hash = do
  result <- case T.unpack hash of
              "SHA256" -> Right $ decrypt Nothing (defaultOAEPParams SHA256) pk (BS.toStrict (toBytes encKey))
              "SHA512" -> Right $ decrypt Nothing (defaultOAEPParams SHA512) pk (BS.toStrict (toBytes encKey))
              _        -> Left "Only sha256 and sha512 are supported"
  case result of
    Left err -> Left $ show err
    Right keyBytes -> case cipherInit keyBytes of
      CryptoPassed key -> Right key
      CryptoFailed err -> Left $ "Couldn't recover ephemeral key " ++ show err


recoverCardData :: AES128 -> Text -> IV AES128 -> Either String CardInfoData
recoverCardData cipher input v =
  let cardData = cbcDecrypt cipher v (BS.toStrict (toBytes input))
      cardDataJson = fromJust . unpad (PKCS7 16) $ cardData -- AES Block Size we use is 16 bytes
  in Json.eitherDecodeStrict' cardDataJson


recoverIv :: Maybe Text -> Either String (IV AES128)
recoverIv Nothing = return nullIV
recoverIv (Just v) = case makeIV (BS.toStrict ((toBytes v) :: ByteString)) of
  Just v' -> Right v'
  Nothing -> Left "couldn't create IV"


toBytes :: Text -> ByteString
toBytes = fst . BS.decode . T.encodeUtf8
