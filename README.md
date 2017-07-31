# MDES stub server #

## Usage ##

 The server requires a private key to run in order to decrypt the encrypted card information received in `/checkEligibility` calls, there's a `key.pem` in the root of this repository that can be used but you have to download it separately if you're running a released binary.

For example:

```sh
$ cd directoy/where/mdes-stub/was-downloaded
$ cp directoy/where/key.pem/was-downloaded .
$ chmod +x ./mdes-stub
$ ./mdes-stub
```

## Usage ##

Run `./mdes-stub -h` for getting help.

## Development ##

You need to have [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed.

### Build ###

```sh
$ stack build
```

### Run ###

```sh
$ stack exec mdes-stub
```

## Stubbed endpoints ##

### `/checkEligibility` ###

This endpoint uses the encrypted key to decrypt the card information and depending on which card was used respond with a different *step up* strategy when calling the `/digitize` endpoint. The encrypted key is recovered with the private key used to run the server so make sure that the encrypted key in question is encrypted with the public key present inside `cert.pem` certificate which is in the root of this repository.

These are the different behaviours depending on which card number used:
- card number `5555 5555 5555 4444` will trigger a step up in the digitize response for the digitize request with the same `appInstanceId`
- card number `5105 1051 0510 5100` or `5200 8282 8282 8210` (or any other card number) will trigger an approved digitization response for the digitize request with the same `appInstanceId`

#### Example request ####

```json
{
  "paymentAppInstanceId": "123123123",
  "cardInfo": {
    "encryptedData": "95B72A4BCF15AA74D3E9415A63EF5BE5083C2B28CC173D4AFAC4646C9EF63CBC4512F84BB608AC1D71F3FD56409BCBBF",
    "oaepHashingAlgorithm": "SHA512",
    "publicKeyFingerprint": "000000",
    "iv": "B5DFABBC7F0EE88CC82518DE98A4065B",
    "encryptedKey": "8F8B823D06EB48370B006D140C3E2521D44645821C924A9CC8A1809170544F91D861B5AA268211B919FA80C1EB0B468C41116F174A2964610723C5FA56472C4BA21FA78F21C0967A2D2F96F9CA5878DFC3DCFE981C49B1C91E8A163D9C588C7A9720702099232E0E316AEC832B571C3AC765A3D2A58C1E62D3199B0706085537F3E548BAFA8828ED96BFC50133E1F2AD904C46DD449D9203517EBDB697AA2419E4C513BD473DD62C2630C6C2399AC7A68B147DD6E70DE6DEF6A7174D88DA4B5D7524D2D68F48B8E7133E3D476DF6507E6FF23933172FDF177A03B6C65003F7917A4DFEFAB93238C19797E52FF3A919A7673F3BCE4E5CBD422A691E70BBC96225527857DEAE01F5E25746684A0E513CAB171A7F138A504C3D43FA481D4F94447F4B3B4B537B5A7D0C32579D560E6343FBA5010E9758CE9856F896BFED77B6108B18E16961EB1050EECDF066A575461E2A45559AE0703113704CB7B0F7B16FD1E52AB143E3ACFBA76926B46D0285EC9959693536B965B3C0DBB4DBB68ECF9D4B812EF25334EB8B33A488AD107BF56AC36072C0DB02F38DD97BEE7DD7D2B5710D37FE2BD0E55D931566C02BA4309CBD9681B6856516B73E226062233B77626A5D4E841CF62CA32F5E4AD99B81A2F02EE84748F891A4B5E3CAF987E074F3D3EC070C823A1AA56744D2DB7CADFC1474FA8893712945D54FDD4E00636D905D243B48C2"
  }
}
```

#### Example response ####

```json
{
  "responseHost": "site1.mastercard.com",
  "responseId": "123456",
  "eligibilityReceipt": {
    "value": "f9f027e5-629d-11e3-949a-0800200c9a66",
    "validForMinutes": 60
  },
  "termsAndConditionsAssetId": "a9f027e5-629d-11e3-949a-0800200c9a66",
  "applicableCardInfo": {
    "isSecurityCodeApplicable": false
  }
}
```

### `/digitize` ###

#### Example request ####

```json
{
  "responseHost": "site1.mastercard.com",
  "responseId": "123123",
  "decision": "REQUIRE_ADDITIONAL_AUTHENTICATION",
  "authenticationMethods": [
    {
      "id": 12344,
      "type": "MASKED_MOBILE_PHONE_NUMBER",
      "value": "12X-XXX-XX32"
    }
  ],
  "tokenUniqueReference": "DWSPMC000000000132d72d4fcb2f4136a0532d3093ff1a45",
  "panUniqueReference": "FWSPMC000000000159f71f703d2141efaf04dd26803f922b",
  "productConfig": {
    "brandLogoAssetId": "800200c9-629d-11e3-949a-0739d27e5a66",
    "issuerLogoAssetId": "800200c9-629d-11e3-949a-0739d27e5a66",
    "iconAssetId": "800200c9-629d-11e3-949a-0739d27e5a66",
    "isCoBranded": "true",
    "coBrandName": "Co brand partner",
    "coBrandLogoAssetId": "dbc55444-496a-4896-b41c-5d5e2dd431e2",
    "cardBackgroundCombinedAssetId": "739d27e5-629d-11e3-949a-0800200c9a66",
    "foregroundColor": "000000",
    "issuerName": "Issuing Bank",
    "shortDescription": "Bank Rewards Mastercard",
    "longDescription": "Bank Rewards Mastercard with the super duper rewards program",
    "customerServiceUrl": "https://bank.com/customerservice",
    "issuerMobileApp": {
      "openIssuerMobileAppAndroidIntent": {
        "action": "com.mybank.bankingapp.action.OPEN_ISSUER_MOBILE_APP",
        "extraTextValue": "ew0KICAgICJwYXltZW50QXBwUHJvdmlkZXJJZCI6ICIxMjM0NTY3ODkiLA0KICAgICJwYXltZW50Q XBwSWQiOiAiV2FsbGV0QXBwMSIsDQogICAgInBheW1lbnRBcHBJbnN0YW5jZUlkIjogIjEyMzQ1Njc 4OSIsDQogICAgInRva2VuVW5pcXVlUmVmZXJlbmNlIjogIkRXU1BNQzAwMDAwMDAwMGZjYjJmNDEzN mIyZjQxMzZhMDUzMmQyZjQxMzZhMDUzMiINCn0=",
        "packageName": "com.mybank.bankingapp"
      }
    },
    "termsAndConditionsUrl": "https://bank.com/termsAndConditions",
    "privacyPolicyUrl": "https://bank.com/privacy",
    "issuerProductConfigCode": "123456"
  },
  "tokenInfo": {
    "tokenPanSuffix": "1234",
    "accountPanSuffix": "6789",
    "tokenExpiry": "1018",
    "dsrpCapable": true
  },
  "tdsRegistrationUrl": "tds.Mastercard.com"
}
```

#### Example response ####

```json
{
  "responseHost": "site1.your-server.com",
  "responseId": 1234
}
```

### `/requestActivationCode` ###

#### Example request ####

```json
{
  "responseHost": "http://localhost:3000",
  "requestId": "123456",
  "paymentAppInstanceId": "123123",
  "tokenUniqueReference": "DWSPMC00000123d72..",
  "authenticationMethod": {
    "id": 12344
  }
}
```

#### Example response ####

```json
{
  "responseHost": "site1.your-server.com",
  "responseId": 1234
}