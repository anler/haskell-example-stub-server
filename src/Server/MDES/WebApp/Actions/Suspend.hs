{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.MDES.WebApp.Actions.Suspend
  ( suspendA
  , unsuspendA
  , deleteA
  , Status()
  ) where

import Web.Scotty
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent (MVar, modifyMVar_)
import Data.Text.Lazy (Text)
import Data.Map.Strict as Map
import Server.MDES.Responses.Common
import Server.MDES.Responses.Suspend
import Server.MDES.Requests.Suspend


makeStatus :: Status -> ReaderT (MVar (Map Text Status)) ActionM ()
makeStatus st = do
  db <- ask
  dat <- lift jsonData
  let tokenRefs = getRefs dat
  liftIO $ modifyMVar_ db (modification st tokenRefs)
  lift $ json $ SuspendResponse $ fmap (toToken st dat) tokenRefs

  where
    getRefs :: SuspendRequest -> [Text]
    getRefs req = req ^. tokenUniqueReferences

suspendA :: ReaderT (MVar (Map Text Status)) ActionM ()
suspendA = makeStatus SUSPENDED


unsuspendA :: ReaderT (MVar (Map Text Status)) ActionM ()
unsuspendA = makeStatus ACTIVE


deleteA :: ReaderT (MVar (Map Text Status)) ActionM ()
deleteA = makeStatus DELETED


toToken :: Status -> SuspendRequest -> Text -> Token
toToken st dat str = Token {
  _tokenTokenUniqueReference = str,
  _tokenStatus = st,
  _tokenSuspendedBy = Just $ dat ^. causedBy,
  _tokenProductConfig = Nothing,
  _tokenTokenInfo = Nothing,
  _tokenTdsRegistrationUrl = Nothing,
  _tokenErrors = Nothing
  }


modification :: Status -> [Text] -> Map Text Status -> IO (Map Text Status)
modification s references var = return $ Map.unions [combined, var]
  where
    combined = Map.fromList $ fmap (\x -> (x, s)) references
