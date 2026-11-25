{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Codec (tests) where

import Cardano.Crypto.Hash (Blake2b_256, hashToBytes, hashWith)
import Cardano.Ledger.Binary (DecoderError, decodeFull, decodeFullAnnotator, decCBOR, serialize')
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (PParamsUpdate, Tx, TxOut, TxLevel (TopTx), eraProtVerLow)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (digitToInt, isSpace)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (^.))
import Proto.Dxdingo.V1.Codec (GovernanceItemKind (..))
import Proto.Dxdingo.V1.Envelope (HarnessErrorCode (..))
import Proto.Dxdingo.V1.Types (Era (..), GovActionType (..))
import Test.Util

hex :: String -> BS.ByteString
hex = BS.pack . go . filter (not . isSpace)
  where
    go (a : b : r) = fromIntegral (digitToInt a `shiftL` 4 .|. digitToInt b) : go r
    go _ = []

toHex :: BS.ByteString -> String
toHex = concatMap byte . BS.unpack
  where
    byte w = [digit (fromIntegral w `div` 16), digit (fromIntegral w `mod` 16)]
    digit n = "0123456789abcdef" !! n

h28, h32 :: BS.ByteString
h28 = BS.replicate 28 0x11
h32 = BS.replicate 32 0x33

-- | Enterprise mainnet key address.
addr :: BS.ByteString
addr = BS.cons 0x61 h28

-- | A Conway transaction [body, wits, true, aux] with the empty body
-- {0: [], 1: [], 2: 0}. Its id is blake2b-256 of the body; the fee-relevant
-- size is body + witnesses + aux + the array header, without the IsValid
-- byte (sizeTxForFeeCalculation).
minimalTx :: BS.ByteString
minimalTx = hex "84 a3 00 80 01 80 02 00 a0 f5 f6"

tests :: [Check]
tests =
  [ ("tx: id, size and validity", txOk)
  , ("tx: IsValid false decodes as invalid", not (tx ERA_CONWAY (hex "84 a3 00 80 01 80 02 00 a0 f4 f6") ^. #codecTx . #isValid))
  , ("tx: trailing byte is rejected", rejected (tx ERA_CONWAY (BS.snoc minimalTx 0) ^. #codecTx . #rejection))
  , ("tx: truncation is rejected", rejected (tx ERA_CONWAY (BS.take 10 minimalTx) ^. #codecTx . #rejection))
  , ("tx: byron is UNSUPPORTED", tx ERA_BYRON minimalTx ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ( "tx: protocol version outside the era is UNSUPPORTED"
    , call (#codecTx .~ (defMessage & #era .~ ERA_CONWAY & #protocolVersion . #major .~ 5 & #cbor .~ minimalTx)) ^. #error . #code
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "tx: protocol version 10 is inside Conway"
    , call (#codecTx .~ (defMessage & #era .~ ERA_CONWAY & #protocolVersion . #major .~ 10 & #cbor .~ minimalTx)) ^. #codecTx . #size == 10
    )
  , ("output: shelley address and coin", shelleyOut ^. #codecTxOutput . #coin == 1000000 && shelleyOut ^. #codecTxOutput . #address == addr)
  , ( "output: mary assets sorted by name"
    , map (\a -> (a ^. #assetName, a ^. #quantity)) (maryOut ^. #codecTxOutput . #assets) == [("aa", 5), ("b", 7)]
        && maryOut ^. #codecTxOutput . #coin == 2000000
    )
  , ("output: inline datum", inlineOut ^. #codecTxOutput . #inlineDatum == hex "d87980" && BS.null (inlineOut ^. #codecTxOutput . #datumHash))
  , ("output: datum hash", hashedOut ^. #codecTxOutput . #datumHash == h32 && BS.null (hashedOut ^. #codecTxOutput . #inlineDatum))
  , ("output: an integer is no output", rejected (output ERA_CONWAY (hex "01") ^. #codecTxOutput . #rejection))
  , ("certificate: stake registration", kindOf ERA_SHELLEY (hex "82 00" <> key) == "RegTxCert")
  , ("certificate: stake deregistration", kindOf ERA_SHELLEY (hex "82 01" <> key) == "UnRegTxCert")
  , ("certificate: Conway tag 7 is RegDepositTxCert", kindOf ERA_CONWAY (hex "83 07" <> key <> hex "1a 001e8480") == "RegDepositTxCert")
  , ("certificate: Conway tag 0 is RegTxCert", kindOf ERA_CONWAY (hex "82 00" <> key) == "RegTxCert")
  , ("certificate: vote delegation is DelegTxCert", kindOf ERA_CONWAY (hex "83 09" <> key <> hex "81 02") == "DelegTxCert")
  , ( "certificate: tag 7 does not exist in Babbage"
    , rejected (cert ERA_BABBAGE (hex "83 07" <> key <> hex "1a 001e8480") ^. #codecCertificate . #rejection)
    )
  , ("governance: info proposal", gov proposalInfo ^. #codecGovernance . #actionType == GOV_ACTION_TYPE_INFO)
  , ("governance: proposal round-trips", gov proposalInfo ^. #codecGovernance . #cbor == proposalInfo)
  , ( "governance: hard fork"
    , gov (proposal (hex "83 01 f6 82 0b 00")) ^. #codecGovernance . #actionType == GOV_ACTION_TYPE_HARD_FORK_INITIATION
    )
  , ( "governance: voting procedures round-trip"
    , call (#codecGovernance .~ (defMessage & #era .~ ERA_CONWAY & #kind .~ GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES & #cbor .~ votes))
        ^. #codecGovernance . #cbor == votes
    )
  , ( "governance: Babbage is UNSUPPORTED"
    , call (#codecGovernance .~ (defMessage & #era .~ ERA_BABBAGE & #kind .~ GOVERNANCE_ITEM_KIND_PROPOSAL & #cbor .~ proposalInfo)) ^. #error . #code
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ("plutus data: constructor", plutus "d87980" == Just (hex "d87980"))
  , ("plutus data: indefinite list keeps its wire form", plutus "9f 01 02 ff" == Just (hex "9f 01 02 ff"))
  , ("plutus data: indefinite bytes keep their wire form", plutus "5f 41 01 41 02 ff" == Just (hex "5f 41 01 41 02 ff"))
  , ("plutus data: general constructor 9 keeps its tag", plutus "d8 66 82 09 80" == Just (hex "d8 66 82 09 80"))
  , ("plutus data: a lone break is rejected", plutus "ff" == Nothing)
  , ("plutus data: trailing byte is rejected", plutus "d87980 00" == Nothing)
  , ("plutus data: text is rejected", plutus "6161" == Nothing)
  , ("header: shelley hash and slot", headerIs ERA_SHELLEY (hex shelleyHeader) "2308cdd4c0bf8b8bf92523bdd1dd31640c0f42ff079d985fcc07c36cbf915c2b" 16156972 2)
  , ("header: conway hash and slot", headerIs ERA_CONWAY (hex conwayHeader) "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47" 159835207 10)
  , ("header: truncation is rejected", rejected (header ERA_CONWAY (BS.init (hex conwayHeader)) ^. #codecBlockHeader . #rejection))
  , ("header: trailing byte is rejected", rejected (header ERA_CONWAY (BS.snoc (hex conwayHeader) 0) ^. #codecBlockHeader . #rejection))
  , ("header: a Shelley header is not Conway", rejected (header ERA_CONWAY (hex shelleyHeader) ^. #codecBlockHeader . #rejection))
  , ( "byron: mainnet block hash, epoch and slot"
    , let r = byron (BS.append (hex "82 01") (hex byronBlock)) ^. #codecByronBlock
       in toHex (r ^. #headerHash) == "1451a0dbf16cfeddf4991a838961df1b08a68f43a19c0eb3b36cc4029c77a2d8"
            && r ^. #epoch == 207
            && r ^. #slot == 7
            && not (r ^. #isBoundary)
    )
  , ("byron: bad tag is rejected", rejected (byron (BS.append (hex "82 02") (hex byronBlock)) ^. #codecByronBlock . #rejection))
  , ("byron: trailing byte is rejected", rejected (byron (BS.snoc (BS.append (hex "82 01") (hex byronBlock)) 0) ^. #codecByronBlock . #rejection))
  , ( "pparams update: min fee A and B only"
    , let u = ppu ERA_SHELLEY "a2 00 18 2c 01 1a 00025ef5" ^. #codecPparamsUpdate . #update
       in u ^. #maybe'minFeeA == Just 44 && u ^. #maybe'minFeeB == Just 155381 && u ^. #maybe'maxTxSize == Nothing && u ^. #maybe'minPoolCost == Nothing
    )
  , ("pparams update: an empty update sets nothing", ppu ERA_ALLEGRA "a0" ^. #codecPparamsUpdate . #update . #maybe'minFeeA == Nothing)
  , ("pparams update: Conway max tx size", ppu ERA_CONWAY "a1 03 19 4000" ^. #codecPparamsUpdate . #update . #maybe'maxTxSize == Just 16384)
  , ( "pparams update: Conway committee size and action lifetime"
    , let u = ppu ERA_CONWAY "a2 18 1b 07 18 1d 06" ^. #codecPparamsUpdate . #update
       in u ^. #maybe'committeeMinSize == Just 7 && u ^. #maybe'govActionLifetime == Just 6 && u ^. #maybe'committeeMaxTermLength == Nothing
    )
  , ( "pparams update: Conway reference-script fee per byte"
    , let r = ppu ERA_CONWAY "a1 18 21 d8 1e 82 0f 01" ^. #codecPparamsUpdate . #update . #minFeeRefScriptCostPerByte
       in (r ^. #numerator, r ^. #denominator) == ("15", "1")
    )
  , ( "pparams update: Alonzo collateral"
    , let u = ppu ERA_ALONZO "a2 17 18 96 18 18 03" ^. #codecPparamsUpdate . #update
       in u ^. #maybe'collateralPercentage == Just 150 && u ^. #maybe'maxCollateralInputs == Just 3
    )
  , ("pparams update: trailing byte is rejected", rejected (ppu ERA_CONWAY "a1 03 19 4000 00" ^. #codecPparamsUpdate . #rejection))
  , ("pparams update: truncation is rejected", rejected (ppu ERA_CONWAY "a1 03" ^. #codecPparamsUpdate . #rejection))
  , ("pparams update: an integer is no update", rejected (ppu ERA_SHELLEY "01" ^. #codecPparamsUpdate . #rejection))
  , -- Divergence confirmations call the reference directly, bypassing
    -- dxnode-hs, so a reported mismatch cannot come from request
    -- translation (dxnode-go TestDivergenceCodec*).
    ( "divergence: an output with an empty multi-asset is re-encoded without it"
    , reencodeOut (hex "82 581d" <> addr <> hex "82 1a1a1e8528 a0") == Right (hex "82 581d" <> addr <> hex "1a 1a1e8528")
    )
  , ( "divergence: a map output with only a datum hash is re-encoded as a legacy array"
    , reencodeOut (hex "a3 00 581d" <> addr <> hex "01 1a0016e360 02 82 00 5820" <> h32)
        == Right (hex "83 581d" <> addr <> hex "1a0016e360 5820" <> h32)
    )
  , ( "divergence: multi-asset order is rewritten, not kept"
    , let wire = hex "82 581d" <> addr <> hex "82 1a001e8480 a1 581c" <> h28 <> hex "a2 41 62 07 42 61 61 05"
       in either (const False) (/= wire) (reencodeOut wire) && either (const False) (const True) (reencodeOut wire)
    )
  , ( "divergence: a three-entry PlutusV1 cost model is rejected in Alonzo"
    , either (const True) (const False) (decodeFull (eraProtVerLow @AlonzoEra) (BSL.fromStrict (hex "a1 12 a1 00 83 01 02 03")) :: Either DecoderError (PParamsUpdate AlonzoEra))
    )
  , ( "divergence: the same cost model is accepted in Conway"
    , either (const False) (const True) (decodeFull v (BSL.fromStrict (hex "a1 12 a1 00 83 01 02 03")) :: Either DecoderError (PParamsUpdate ConwayEra))
    )
  , ( "divergence: a three-element Conway transaction is accepted and re-encoded with IsValid"
    , reencodeTx (hex "83 a3 00 80 01 80 02 00 a0 f6") == Right (hex "84 a3 00 80 01 80 02 00 a0 f5 f6")
    )
  ]
  where
    ppu e h = call (#codecPparamsUpdate .~ (defMessage & #era .~ e & #cbor .~ hex h))
    tx e b = call (#codecTx .~ (defMessage & #era .~ e & #cbor .~ b))
    output e b = call (#codecTxOutput .~ (defMessage & #era .~ e & #cbor .~ b))
    cert e b = call (#codecCertificate .~ (defMessage & #era .~ e & #cbor .~ b))
    header e b = call (#codecBlockHeader .~ (defMessage & #era .~ e & #cbor .~ b))
    byron b = call (#codecByronBlock .~ (defMessage & #protocolMagic .~ 764824073 & #cbor .~ b))
    gov b = call (#codecGovernance .~ (defMessage & #era .~ ERA_CONWAY & #kind .~ GOVERNANCE_ITEM_KIND_PROPOSAL & #cbor .~ b))
    rejected r = [(f ^. #rule, f ^. #name) | f <- r ^. #failures] == [("codec", "DeserialiseFailure")]
    txOk =
      let r = tx ERA_CONWAY minimalTx ^. #codecTx
       in r ^. #txId == hashToBytes (hashWith @Blake2b_256 id (hex "a3 00 80 01 80 02 00"))
            && r ^. #size == 10
            && r ^. #isValid
            && r ^. #cbor == minimalTx
    key = hex "82 00 581c" <> h28
    kindOf e b = cert e b ^. #codecCertificate . #certKind
    shelleyOut = output ERA_SHELLEY (hex "82 581d" <> addr <> hex "1a 000f4240")
    maryOut = output ERA_MARY (hex "82 581d" <> addr <> hex "82 1a001e8480 a1 581c" <> h28 <> hex "a2 41 62 07 42 61 61 05")
    inlineOut = output ERA_BABBAGE (hex "a3 00 581d" <> addr <> hex "01 1a0016e360 02 82 01 d8 18 43 d8 79 80")
    hashedOut = output ERA_BABBAGE (hex "a3 00 581d" <> addr <> hex "01 1a0016e360 02 82 00 5820" <> h32)
    reward = hex "581d e0" <> h28
    anchor = hex "82 75" <> "https://example.org/p" <> hex "5820" <> h32
    proposal action = hex "84 1a3b9aca00" <> reward <> action <> anchor
    proposalInfo = proposal (hex "81 06")
    votes = hex "a1 82 02 581c" <> h28 <> hex "a1 82 5820" <> h32 <> hex "00 82 01 f6"
    plutus s =
      let r = call (#codecPlutusData .~ (defMessage & #cbor .~ hex s)) ^. #codecPlutusData
       in if null (r ^. #rejection . #failures) then Just (r ^. #cbor) else Nothing
    headerIs e b h slot pv =
      let r = header e b ^. #codecBlockHeader
       in toHex (r ^. #headerHash) == h && r ^. #slot == slot && r ^. #protocolVersion . #major == pv && r ^. #cbor == b
    reencodeOut :: BS.ByteString -> Either DecoderError BS.ByteString
    reencodeOut b = serialize' v <$> (decodeFull v (BSL.fromStrict b) :: Either DecoderError (TxOut ConwayEra))
    reencodeTx :: BS.ByteString -> Either DecoderError BS.ByteString
    reencodeTx b = serialize' v <$> (decodeFullAnnotator v "Tx" decCBOR (BSL.fromStrict b) :: Either DecoderError (Tx TopTx ConwayEra))
    v = eraProtVerLow @ConwayEra

shelleyHeader :: String
shelleyHeader =
  concat
    [ "828f1a004d4a6f1a00f6892c5820bc4766a289bb5d8ec86647a0aeed5dc3521f43db7a72abef3eed1debbfa9412f5820ddba"
    , "672a2abc65da47537df8f190ba376512bc0283e731e3934b52f1a0abc1a558206abad5392188997c5e14b9f6e581129ec141"
    , "cec17d1d2f0cfc1e9192d464716e825840df9856406b92387c9b138ef24a58d8c48a37bf5be8721eaec4702ca98df48edd26"
    , "78f0a8c722982d354582b5da65d849f72077c880778982f71de60536c3ed4a5850edc5d9f0ea6965edf729548ff7cba2cea8"
    , "2425ceb4e42d1074a726cc9812eb5fcabb4aefa7f049bcc5209edbbe18763a080eb1a0bdfd8fb933a1763813dd3314c22984"
    , "7a880116bee73c97e24130f1058258400009fa6d8e070a89d1ce903f49868682ff8dec52a0756ef60cb99226b36ab781fa8e"
    , "0fa296b286b964d9cac56517071501716b752dc11636a5f9b8c29193012d58505a32d1a3ce3a8a89f17c73aedbab2c299a52"
    , "f90fd78adf78fe6b5ac8cfded66ebd6d2fb238a770b6d0f022954f59855138c8ec47dc1021b8b69dabf26592076ffd612d04"
    , "bfbc8fbcfdaade7d7eda670f19089958204847b0787633b18df8724c920c7cc35172531f2756fe4d6ab17489e019c2c4f358"
    , "201ee99223a40caf33ca63758235af29c1d53066a423249f5690c8f9e664924dcf00186158404e4b58717c4f894e34c41d5b"
    , "3d68de6676f78d9a5d719a077e282974cb136bcc6287d3fbf76e175a76edce3eb934afcf7005f5a7cc9204adf14fa9d01c12"
    , "3a0902005901c09b1bb23288fa4ba5f8ad23d00d0d945e89958057c17df6557a37a331867a6ef252a7893bfcf7006e32a614"
    , "a1ff9787e7af3575cfdeb5054214ded324fa850b090f292bd54599e88bd4efe12b49d520eb7a8e57270dbd532dc567fdb8e5"
    , "cfc386e1e28c1812734dfcde9dede86845d8e66966089f47b410225886e125942b445f912d70e517d175e9ee6f4de13f0aad"
    , "f3a16e01911875df1710b49ea063d17228297c9eabf6cf663e7f5f5e58f6e86bfe7117381070c86b42981bcc7d405a49108f"
    , "dd760f61815e4957ad256136706e94587ccac9ded8f9459b9433d0b6d06df5e3a821a715ee5fd912bb63d26aa1365ddf0dbf"
    , "f435ab42252cb2e9549f41bda074201f824cd48b6dbc8c9d79961b9c7f0cab85caa2bedef282c585ee0cad01b771a48f6740"
    , "e0115acc27b4a8bb998b0f05de1cfef0b2507cd2f4d65120389fb88513e03c8a1709628faf3e48bb3956de316198ee06d45c"
    , "e84b218632dd53f3ab0087681c90287c14c37ad76b17bbbeb68bb4924363987355859d6b6098418b2117b01bab13a786555d"
    , "4e996dff6e1510f2e11070aa9223a229f69f5fd503de22b09052cb6e4f371f8a8858258df1cc5ebde829a8befce981f4fbaa"
    , "e621f8917e"
    ]

conwayHeader :: String
conwayHeader =
  concat
    [ "828a1a00b82b211a0986e4475820ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a05820dbdc"
    , "e19c856881a7179d61666204368a13e282b0f473b6dee7a79c44e47a11c65820113752032b1036bd7ab48b47302a723bf723"
    , "8bbc08e798f56d1bb9a205679929825840b7e53621a22d79026e0c21f4569019713997d7f3fcaaafdea9099949ca9ada9051"
    , "02e166a79ae5c1b5b5e25807c08131b4b88f241a2953e7379a09f1d1e7c7205850d6bf53d7374e4e8af248156c381102be6d"
    , "928af050d914f27dd47fa3333b055cc500abed018ddd680836f1cbac8b4ea2690ce2ec8d597e096a210a9335a97e3c1bf93b"
    , "03166b2657dd79d8754cbf320a191aaa5820e269b23136f37156fe832904eff083e175059de240544e0b439d352c828c24e3"
    , "845820cb7a64398b69f2ff3f17fccd154ccf079557ac15d7e57a7ef58ad9bbe87bcead18181904c858401830540aa9112273"
    , "76268a31cfd6e37c891fe7fe0a869005cf0d82f3075b57f838e10c0f8be162308b24efb6487ca74ba714adf1e39f22db25f7"
    , "61cb7726a60e820a025901c0f77d128743cfefaa320444914a7ca99e451859f80bb845225246096bf5e009b2af36707c0426"
    , "2abf1205022b9382eb8190ec388225a63cccfd6d216aaa2daa0d89c266d875ee0ba9269c936bda8888cee3f024b8276c427b"
    , "aa2cf880c919fd7a03aa896dc56d127cf4907ff823bf4d6a5de137e8c76320d9f45e12a21539d18afd94240916240a5f5ee5"
    , "845cf57f9fd73f8ca6d309cb89369b316867e922f381c0af13614004a165c4076486f5e42823586e01924f82d8ca3b7a1846"
    , "91f00521934f9b41919065725dce314aaef2a5287d6f7a8c3825afcc0eaa60cd159e3f8f7f6cd9bfbbe100d0462048fdb365"
    , "49f41b4482231117b91c1219d6f4499eb037c758f582d0a2e8068e645d14b05a108968dc1643fd82dc6568b3c281d42577da"
    , "1836bcb89c8a9d60dd945fd4870513c231e83d203939116ff2875f5b464744ea9678b2aa868a2272827320b6a8f21cccbb14"
    , "bd3d700ce2f85e824dae0fc4204606051b7884229d1734eb28ce0eb7a445a5c9b8dff2784a132ca1b57c10ef63b57cf1393f"
    , "c07a8d4ea4e9f10ec32bf789bc696f9404b63406d583e981390f3c15b67e0f7f6f9ac440f7c6ece8cea6dd3e44e34cca6035"
    , "323dd3d22f95eb89182a"
    ]

byronBlock :: String
byronBlock =
  concat
    [ "83851a2d964a09582025df38df102b89ec25a432a2972993d2fa8cc1f597a73e6260b2f07e79501eb084830258200f284bc2"
    , "2f5b96228ee0687b7bb87c56132f77df4235c78a1595729ccfce2001582019fb988d02ec920a6de5ac71c5d5e75f8b73d7ed"
    , "8e8abea7773e28859983206e82035820d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c5820"
    , "afc0da64183bf2664f3d4eec7238d524ba607faeeab24fc100eb861dba69971b58204e66280cd94d591072349bec0a3090a5"
    , "3aa945562efb6d08d56e53654b0e4098848218cf0758401bc97a2fe02c297880ce8ecfd997fe4c1ec09ee10feeee9f686760"
    , "166b05281d6283468ffd93becb0c956ccddd642df9b1244c915911185fa49355f6f22bfab9811a004430ed82028284005840"
    , "1bc97a2fe02c297880ce8ecfd997fe4c1ec09ee10feeee9f686760166b05281d6283468ffd93becb0c956ccddd642df9b124"
    , "4c915911185fa49355f6f22bfab9584061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a885d0"
    , "054af7c23fc6c37919dba41c602a57e2d0f9329a7954b867338d6fb2c9455840e03e62f083df5576360e60a32e22bbb07b3c"
    , "8df4fcab8079f1d6f61af3954d242ba8a06516c395939f24096f3df14e103a7d9c2b80a68a9363cf1f27c7a4e30758403250"
    , "68a2307397703c4eebb1de1ecab0b23c24a5e80c985e0f7546bb6571ee9eb94069708fc25ec67a4a5753a0d49ab5e536131c"
    , "19c7f9dd4fd32532fd0f71028483010000826a63617264616e6f2d736c01a058204ba92aa320c60acc9ad7b9a64f2eda55c4"
    , "d2ec28e604faf186708b4f0c4e8edf849f82839f8200d8185824825820b0a7782d21f37e9d98f4cbdc23bf2677d93eca1ac0"
    , "fb3f79923863a698d53f8f018200d81858248258205bd3e8385d2ecdd17d3b602263e8a5e7aa0edb4dd00221f369c2720f7d"
    , "85940d008200d81858248258201e4a77f8375548e5bc409a518dbcb4a8437b539682f4e840f4a1056f01cea566008200d818"
    , "58248258205e83b53253f705c214d904f65fdaaa2f153db59a229a9cee1da6c329b543236100ff9f8282d818584283581ca1"
    , "932430cb1ad6482a1b67964d778c18b574674fda151cdfa73c63cda101581e581cfc8a0b5477e819a27a34910e6c174b50b8"
    , "71192e95cca1a711bbceb3001abcb52f6d1b000000013446d5718282d818584283581c093916f7e775fba80eaa65cded085d"
    , "985f7f9e4982cddd2bb7c476aea101581e581c83d3e2df30edf90acf198b85a7327d964f9d92fd739d0c986a914f6c001a27"
    , "a611b61a000c48cbffa0848200d8185885825840a781c060f2b32d116ef79bb1823d4a25ea36f6c130b3a182713739ea1819"
    , "e32d261db3dce30b15c29db81d1c284d3fe350d12241e8ccc65cdf08adba90e0ad4558408eb4c9549a6a044d687d6c04fdee"
    , "2240994f43966ef113ebb3e76a756e39472badb137c3e0268d34ce6042f76c2534220cc1e061a1a29cce065faf486184cf07"
    , "8200d818588582584085dc150754227f68d1640887f8fa57c93e4cad3499f2cb7b5e8258b0b367dcceaa42bf9ea1cfff73fd"
    , "0fab44d9e0a36ef61bc5d0f294365316a4e0ed12b40a135840f1233519fa85f3ecbb2deaa9dff2d7e943156d49a7a3360338"
    , "1f2c1779b7f65ea0d39a8dcdd227f5d69b9355ab35df0c43c2abb751c6dd24b107a2c7ac51f5088200d81858858258403559"
    , "467e9b4a4e47af0388e7224358197e5d39c57c71c391db4a7d480f297d8b86b0746de21dc5dfca2bd8b8fa817c1fa1c3bd3e"
    , "eaddbfd7a6b270564e416d0c5840b0e33544dcb1895b592a612f5be81242a88226d0612da76099b653f89ce7c5641af14fad"
    , "696ccd44b58744915291240224fd83a26f103c0717752ea256b4af0b8200d8185885825840572c3ea039ded80f19b0d6841e"
    , "9ad0d0d1b73242ac98538affbec6e7356192f48eba0291ea1b174f9c42e139ba85ce75656a036ba0993dda605d5a62956dba"
    , "6558406257e3a27a896268cade4d5371537ed606d3004d6269f87ebe6056b6eff737a2a9ef82d27ba1f9b642ffc622ec27b3"
    , "8e69ed41e272d3de0767cad860d50fa10d82839f8200d8185824825820779a319e0d64b80eaff5ed13d08062b8672fc71ac2"
    , "7e7b30574c1c7972764de202ff9f8282d818582183581c2c0dd53d4e6001e006729fc09d74c5a799d5f93c9f4b74748412a8"
    , "23a0001a1abd89081a769cfd808282d818582183581c05b073f36ee030589a31148838cd47e8d8c8f82fec9fe091c7d53cd8"
    , "a0001a0c5f26b11b00000016bc0c4c47ffa0818200d8185885825840f129f07bbfd87fd1d3ff5fb32e9a5566e02208f89518"
    , "e9994048add22074f433424e682a392581268c7544e34e9c54378a8820bdcf7dddce30490bbb2d363b4b5840709a2e70d380"
    , "3554a15d788235bf56c9567407102be375be5071fa81d4c137047743b5f5abefdbab6b2781822474995dff917213c962ecd1"
    , "11619d75b8534f0aff8203d90102809fff82809fff81a0"
    ]
