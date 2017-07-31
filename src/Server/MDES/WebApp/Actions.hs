{-# LANGUAGE OverloadedStrings #-}

module Server.MDES.WebApp.Actions
  where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Aeson                  (toJSON)
import           Data.String                 (fromString)
import qualified Data.Text.Lazy              as T
import qualified Network.Wreq                as HttpClient
import           Server.MDES.Requests.Common
import           System.Log.Logger
import           System.Random               (randomRIO)
import           Web.Scotty


asyncResponse :: ToJSON a => String -> a -> ActionM ()
asyncResponse endpoint requestBody = do
  dat <- jsonData
  let host = (dat :: Common) ^. responseHost
      url = (T.dropWhileEnd (== '/') host) `T.append` fromString endpoint
  liftIO $ forkIO $ do
    latencySeconds <- randomRIO (0, 12)
    threadDelay $ latencySeconds * 1000000
    HttpClient.post (T.unpack url) (toJSON requestBody)
    infoM "default" $ unwords [ "✓ Sent async response to "
                                , T.unpack url
                                , " after "
                                , show latencySeconds
                                , " seconds."
                                ]
    return ()
  return ()
