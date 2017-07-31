{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.MDES.Responses.Provision
  ( provisionData
  , Provision()
  ) where

import           Data.Aeson.Extras (makeFieldsOption)
import           Data.Aeson.TH
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Text         (Text)
import           GHC.Generics


data Provision = Provision
  { _provisionResponseHost         :: Text
  , _provisionRequestId            :: Text
  , _provisionPaymentAppProviderId :: Text
  , _provisionPaymentAppInstanceId :: Text
  , _provisionTokenUniqueReference :: Text
  , _provisionTokenType            :: Text
  , _provisionTaskId               :: Text
  , _provisionApduCommands         :: [Map Text Text]
  , _provisionSeId                 :: Text
  } deriving (Generic, Show)


deriveJSON (makeFieldsOption "_provision") ''Provision

provisionData :: Provision
provisionData = Provision
  { _provisionResponseHost = "site1.mastercard.com"
  , _provisionRequestId = "123456"
  , _provisionPaymentAppProviderId = "123456789"
  , _provisionPaymentAppInstanceId = "123456789"
  , _provisionTokenUniqueReference = "DWSPMC000000000fcb2f4136b2f4136a0532d2f4136a0532"
  , _provisionTokenType = "EMBEDDED_SE"
  , _provisionTaskId = "3dacc64b-9703-4201-af40-02e636e3ff3b"
  , _provisionSeId = "824bb0419714df99bc5095dd"
  , _provisionApduCommands =
      [ Map.fromList [ ("messageId", "00000000000000000000000000000001"), ("apduCommand", "00A4040008A00000000410101100")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000002"), ("apduCommand", "8050220008112233445566778800")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000003"), ("apduCommand", "848203001001E48FEB145FD5F94B8DB3F26F05BE3F")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000004"), ("apduCommand", "80E29000A078579EA7857437E55574B3D197DA5EF400C7E53BD88CF8225B063CC6CEB4B904E5AAB1ED4D4D9C96DEA3830D81B5BDF17AB85BD33280DC4231BD1653D26F38AFB81E516223078EED6885226EBA3398D3B96264764D3E8462AFB3E33D8AB5A1BE6487E52FFC45EDEE6537C7EFDEB63D251C47FAE7E26F7BAD94B862DE6DFA73F61F5C124868DF21AFCB75C96CE589458EB236356E5792B2A1805C289A8B811F1B")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000005"), ("apduCommand", "80E29000B0C1331181D38100829250757ECFA55538C1E187485B872628D76E7CD49D49BF7F3F8D9554C12F0F039564CBF8492167E3FB11E223B88249DCA6A4747B5133CCF96C65D39C0FA75ABD5244BEFD389999569916C3DF2D4C0EF22D54AF5C6BB7A324F681C0D1F0711E22AC0C4B81D744566BB601A5BFCB9142A40810716AC2806B64AD96348182C22E59B7B107AF7898AFFE86124F563BD1C9A9DB3ECC9C4D3577A789422B4AE4E1C0255812DEEECF9079DC")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000006"), ("apduCommand", "80E29000F0507E1362F1D8F77D86D96C96A32BAA3A3D726914B43F2760F220D3BC5BAD586749C942DC5812974D8001E9969268A2A1918116DA3E2C3B01C11B7213E076BD6A675B139E4F3EAC6933DD8CFD15F58D1031C7619CC5F8E1D598195568DE14C55B87EC1CF70A52D0E245148FD5D4D5F959D2C68479BB3E2954D3F75D793F8A9F560E00D2F913CFDE0567E1994AF37858E31A017A500E22406470F30A9DB7B6440BC2937CE8AE3C1334DD93D5A9E0E6E211857C8EC326FB1CCD79908618C0901C0B57956174141C69C65DE0B8518628FC15A6A362B7547F94D34B923A42A315ADB1EA2DBA7F14917EAD62ED793E9880B25D")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000007"), ("apduCommand", "80E29000D02FAC49A4150591682F812BA69284FDE5E5900CB3A8CEA705DC30119479EC0A302A651354071817192AB86822040769BFB24649B43DAF45F9948F0221E40A6883A30C4CACA275323E088ED3373DDBBEAA43094E6D9A96EEBB2B8F2D7A965162139ADDE7751E688BEC8EC2E4AD2DF55E898C651B1BF6E100E3E8380338F6B94188F6F1819A0E9BB3219499554B4C04D06A7100C1FC4ED782D0DC7CA72A080BF47A35951F1EB2F1A8EC0712C8B272E8B48533981598BD3DE6A7429DC6EB856EA79AB8ABB19C32676679B1CBF5F596181227")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000008"), ("apduCommand", "80E2900060752C9401B783030379E5482F0EE51ACBE52010EB9476B35E49DD3A6E0DF334581E16BAFE508C03371215AD99E2B17854BCF8ED689DF50C7562A01F571B5F5AC1A42921B3B898CF163AF7AC47ED272A5C0CD922DFB11268E38BCD3D60EC368FD8")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000009"), ("apduCommand", "80E29000500E3231EE65304BA4DA5210736372E2E1DDFE34222BD4BD3BD5AF0D190A61D61C9F99E181A9D94E3A3B6F8FAC5A89C8C69CEC7FE9AA6CE99CB67416FC673F96A9A0605BAF52ABB7916615E47DC7C671C0")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000010"), ("apduCommand", "80E2900030A238611C2FBC78916A25F15BF9679ABA4259D78EB26ED6FCB31443E67FA4B61C8F9957985E90B80BE74B1C97B60621EE")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000011"), ("apduCommand", "80E2900040D1233B38BFE2211C91AC7F770721E2CB53BD2D3C3D702E10C2295EB773CD839BCDA05337DBAFDA6C47B302AB06F9180E25FBB0C5266C7B31D4439D0B5DA1ACDA")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000012"), ("apduCommand", "80E29000484E47828845A4BF9728BD770F76D69816F288E17E1F12188CC4E4F0AEC31D4F73EF7BB1B0D2CC97F3E50AB6D67BC5763E322C6499A0356E57596331E46E857FB12D5822348EDECA78")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000013"), ("apduCommand", "80E2900028ADB23F4267AD793B84229CFEAAE5149401C4266BA945BF6AE56F07647777FEC80BBEFA8388B59A30")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000014"), ("apduCommand", "80E2900018C8AA52A8BEEDEA23B68CC41C5910A74058F78FA02596E1F4")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000015"), ("apduCommand", "80E29000A06D7F280CBE8F4CD992310367DDB17C674364BFE3E69B35C34172EAE52A4D21AE4219273448C6BA2FD84341701510E695E5C6D97BB76C6A0FA29C092BF96F6D1A2CA19B49F04C439695B2338CF6DD269515D241B5C7DBFAFF15BA9909B84E7A799EAAD1AAA6A5A29F5D4D3E698DBC93420ADDB129CB5B9BA7934345D1265CCC15234DB982A2ABC2075E2D9F8A081D57AF49E6FBB4615064CA88D2DCB47FC03F02")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000016"), ("apduCommand", "80E290001815F54A260B2DFEE2D79B755E7764A3E76A0EA735A83DB527")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000017"), ("apduCommand", "80E29000207B1735258634C42CF2FC50F76F72B382786EEF1230882AD112C4A03411537E57")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000018"), ("apduCommand", "80E29000185225E425BF2AC84F5B0EF75A69C812A806BA1DCB5B8EBEAB")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000019"), ("apduCommand", "80E2900040EA8BA3A1A31117887E8C9C2498647F8E0CF85E957E51363039C5916ACBE832E1FA03B74046A67FCC4E6B66D15ACDC5E803B6B025AF467AA0BC8C7AB25A078E1D")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000020"), ("apduCommand", "80E29000202562C45C4E99D007B5CD5DFCE24B6B8F0482D1AF56C08166796FD8F6F940E73B")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000021"), ("apduCommand", "80E29000201F3E80816986318E2D13D187C4BA4DE793EF88B5C0A0ADF8604A9AED3F232A67")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000022"), ("apduCommand", "80E29000109C78F3F1373E80F438A9A298D6B647C7")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000023"), ("apduCommand", "80E2900010E6A68A1348AD5651963D7E3C5930739A")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000024"), ("apduCommand", "80E2900010A63C3FE002DA5FAC625519B62CEA406A")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000025"), ("apduCommand", "80E29000188BA9B638031A8C61CA0F4FBCB893592ADBD76A3D23C5A075")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000026"), ("apduCommand", "80E2900030B9699D107E8B78BF379AA2A2932C4B11E8A315DFED6522A1DDA0271A8E837680D9D9F9AB7EEE5BE617DE6CD1FB17D36D")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000027"), ("apduCommand", "80E29000189A217B025E303AF8BBB1FCBBF7F8B87C3BFAB1E23837BBD1")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000028"), ("apduCommand", "80E290001870076A97259038F8ADC038D42E832F39359A4EF6FFDC0C66")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000029"), ("apduCommand", "80E2900050E50A719D66FC253F797CCD199DA6E33888D060062CB5DB7E994DA8BDFCD6A5EEB94A8937462D41369B3D7F213930F644FF0169F8FB95CE7ED362B20C983546EBA7BED6DD13EACBCD58EBF0312BACBE84")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000030"), ("apduCommand", "80E2900050CC6359E0A500EE1C6F25F8A68BD4930CD51ECF83D33F56D7CA2112B0F955235A831829A2B8EC987A107B8C78BA6915C81FA8CCC8E2EEE58689B120AD1451F27D95CE4015D788512A06337093AE492B9F")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000031"), ("apduCommand", "80E2900050FD141DDEC21496C79DD50B816A297E40260716846020F12746A279BA2483A9D5C86ABE2945C52B1BA646C05EE1247F31CD2B4C664F579D44525F8B41524D735AB7558DC1A0169AAE3280AF93F0AB71D3")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000032"), ("apduCommand", "80E2900050D9E00E16D6FCB215D027C4F6CC0B5CE1E7DD559D7B2B2E20083E2E60ABE1EA870F2D5CDFC89B3D6F65AC4F6F8536F4E25530BE4F81EE17C27F5F7B4F47AE9AF27DFD315E3C32F32FE4535CFBA0E17CE6")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000033"), ("apduCommand", "80E2900050D56A25775CB0D45737B02D460E5B608020232FF4BC6A808A61DBC16FA886903D8A311666A4DE7D37718B861D83AED316EC1FA9AF074883D44BC447EAA653DADDA696C4CB4F2F67788BE3061BCA255607")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000034"), ("apduCommand", "80E29000201D0039BFA4286D248BE3FBEABED43F0B81238AD91AAD8B972F3DEA2C7F6D9E85")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000035"), ("apduCommand", "80E29000187802AE57388FB553168C36CEE28503A8A7D4F452F392F28B")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000036"), ("apduCommand", "80E29000108D0D18709014AD8D561DB292AA72FD3E")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000037"), ("apduCommand", "80E29000180725FDB1DCFD4EDEE53DE0564AD2D87AC6DC23FEAC2F7B2B")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000038"), ("apduCommand", "80E290002002407E59859012BA4D38538D80B855FF58BBBAE21D86E8401E9AC3F276698AF7")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000039"), ("apduCommand", "80E2900018B641559BB1CF6D89DB19D1ED6B2420576D95DCDCB5812D19")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000040"), ("apduCommand", "80E29000180B1F0ABD4D5A808F2531315D2FD1211444FAAD737E7298B6")]
      , Map.fromList [ ("messageId", "00000000000000000000000000000041"), ("apduCommand", "80E290004056BBAF7FAF825D12DE348EBE39A0E2941218502AA83223A26722FC16F561097DD50F4F9F19E96E1CB9C85F9D966A766DF37E2E3B48F7BB46CD804B47BC206305")]]
  }
