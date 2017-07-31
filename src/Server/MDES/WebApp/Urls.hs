module Server.MDES.WebApp.Urls
  (
    checkEligibility
  , digitize
  , asset
  , requestActivationCode
  , suspend
  , unsuspend
  , delete
  , activate
  ) where

checkEligibility, digitize, requestActivationCode, asset, suspend, unsuspend, delete, activate :: String

checkEligibility = "/checkEligibility" `withApi` "digitization"

digitize = "/digitize" `withApi` "digitization"

asset = "/asset" `withApi` "assets"

requestActivationCode = "/requestActivationCode" `withApi` "digitization"

activate = "/activate" `withApi` "digitization"
suspend = "/suspend" `withApi` "digitization"
unsuspend = "/unsuspend" `withApi` "digitization"
delete = "/delete" `withApi` "digitization"

withApi :: String -> String -> String
withApi url api = "/mdes/" ++ api ++ "/1/0" ++ url
