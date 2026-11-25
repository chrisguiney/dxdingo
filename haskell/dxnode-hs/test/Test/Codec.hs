{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Codec (tests) where

import Cardano.Crypto.Hash (Blake2b_256, hashToBytes, hashWith)
import qualified Cardano.Chain.UTxO as ByronUTxO
import Cardano.Ledger.Binary (byronProtVer, decodeFullAnnotatedBytes)
import Cardano.Ledger.Binary (DecoderError, decodeFull, decodeFullAnnotator, decCBOR, serialize')
import Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytes, hashSize)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (BlocksMade (..), Network (..), StrictMaybe (..), TxIx (..))
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError, knownNonZeroCoin, knownNonZeroCompactCoin)
import Cardano.Ledger.Compactible (CompactForm)
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Hashes (KeyHash (..), VRFVerKeyHash (..), unsafeMakeSafeHash)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (EpochState (..), LedgerState (..), NewEpochState (..), UTxOState (..))
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts, registerShelleyAccount)
import Cardano.Ledger.State (ActiveStake (..), ChainAccountState (..), DState (..), EraAccounts (..), EraCertState (..), PState (..), SnapShot (..), SnapShots (..), StakePoolState (..), StakeWithDelegation (..), UTxO (..), unUTxO)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (inject)
import Cardano.Ledger.BaseTypes (BoundedRational (..))
import Cardano.Ledger.Address (AccountId (..))
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.MemPack as MemPack
import qualified Data.ByteString.Short as SBS
import Cardano.Ledger.Alonzo.TxOut (dataHashTxOutL)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word8)
import Cardano.Ledger.Binary (DecCBOR)
import qualified Cardano.Ledger.Core as L
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraBlockBody)
import Cardano.Ledger.Dijkstra.Tx (Tx (..))
import qualified Data.Text as T
import qualified Data.VMap as VMap
import Cardano.Ledger.Core (PParamsUpdate, TxCert, TxOut, TxLevel (TopTx), eraProtVerLow)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (digitToInt, isSpace)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Micro ((&), (.~), (%~), (^.))
import Proto.Dxdingo.V1.Codec (CodecLedgerStateResponse, GovernanceItemKind (..), LedgerStateFormat (..))
import qualified Proto.Dxdingo.V1.State as S
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

-- | A hash of one repeated byte.
hashOf :: forall h a. HashAlgorithm h => Word8 -> Hash h a
hashOf b = fromMaybe (error "hash") (hashFromBytes (BS.replicate (fromIntegral (hashSize (Proxy @h))) b))

cc :: Integer -> CompactForm Coin
cc = compactCoinOrError . Coin

poolKey :: KeyHash r
poolKey = KeyHash (hashOf 0x77)

stakeCred :: Credential r
stakeCred = KeyHashObj (KeyHash (hashOf 0x01))

-- | A representative NewEpochState of an era: epoch 7, pots, one output, one
-- pool, one snapshot entry, blocks made, and the account hook.
sampleNes ::
  forall era.
  (L.EraTxOut era, EraCertState era, EraAccounts era, Default (NewEpochState era)) =>
  [Word16] ->
  (Accounts era -> Accounts era) ->
  NewEpochState era
sampleNes ixs acctHook =
  nes0
    { nesEL = EpochNo 7
    , nesBprev = BlocksMade (Map.fromList [(poolKey, 3)])
    , nesBcur = BlocksMade (Map.fromList [(poolKey, 5)])
    , nesEs = es1
    }
  where
    nes0 = def :: NewEpochState era
    es0 = nesEs nes0
    ls0 = esLState es0
    us0 = lsUTxOState ls0
    txin ix = TxIn (TxId (unsafeMakeSafeHash (hashOf 0x42))) (TxIx ix)
    out = adaOut @era
    cs0 = lsCertState ls0
    pool =
      StakePoolState
        { spsVrf = VRFVerKeyHash (hashOf 0x55)
        , spsPledge = Coin 71343432
        , spsCost = Coin 400249652
        , spsMargin = fromMaybe (error "margin") (boundRational 0.05)
        , spsAccountId = AccountId (KeyHashObj (KeyHash (hashOf 0x66)))
        , spsOwners = Set.fromList [KeyHash (hashOf 0x66)]
        , spsRelays = mempty
        , spsMetadata = SNothing
        , spsDeposit = cc 500000000
        , spsDelegators = Set.empty
        }
    cs1 =
      cs0
        & certPStateL %~ (\ps -> ps {psStakePools = Map.fromList [(poolKey, pool)]})
        & certDStateL %~ (\ds -> ds {dsAccounts = acctHook (dsAccounts ds)})
    markSnap =
      SnapShot
        (ActiveStake (VMap.fromList [(stakeCred, StakeWithDelegation (knownNonZeroCompactCoin @40) poolKey)]))
        (knownNonZeroCoin @40)
        VMap.empty
    es1 =
      es0
        { esChainAccountState = ChainAccountState (Coin 1000) (Coin 2000)
        , esLState =
            ls0
              { lsUTxOState = us0 {utxosUtxo = UTxO (Map.fromList [(txin ix, out) | ix <- ixs]), utxosFees = Coin 194490}
              , lsCertState = cs1
              }
        , esSnapshots = (esSnapshots es0) {ssStakeMark = markSnap}
        }

-- | A pre-Conway account: registered with a deposit, delegated to the pool.
shelleyAccounts :: ShelleyEraAccounts era => Accounts era -> Accounts era
shelleyAccounts = registerShelleyAccount stakeCred (Ptr minBound minBound minBound) (cc 2000000) (Just poolKey)

-- | The era's state with the given output indexes, one output each.
nesFixture :: [Word16] -> Era -> Maybe BS.ByteString
nesFixture ixs = \case
  ERA_SHELLEY -> Just (enc (sampleNes @ShelleyEra ixs shelleyAccounts))
  ERA_ALLEGRA -> Just (enc (sampleNes @AllegraEra ixs shelleyAccounts))
  ERA_MARY -> Just (enc (sampleNes @MaryEra ixs shelleyAccounts))
  ERA_ALONZO -> Just (enc (sampleNes @AlonzoEra ixs shelleyAccounts))
  ERA_BABBAGE -> Just (enc (sampleNes @BabbageEra ixs shelleyAccounts))
  ERA_DIJKSTRA -> Just (enc (sampleNes @DijkstraEra ixs id))
  _ -> Nothing
  where
    enc :: forall era. (L.Era era, EncCBOR (NewEpochState era)) => NewEpochState era -> BS.ByteString
    enc = serialize' (eraProtVerLow @era)

-- | MemPack TxOut rows, one per era: ada-only for the eras before Mary, assets
-- in Mary, a datum hash in Alonzo and Babbage.
mempackFixtures :: Era -> [(String, BS.ByteString)]
mempackFixtures = \case
  ERA_SHELLEY -> [("ada", MemPack.packByteString (adaOut @ShelleyEra))]
  ERA_ALLEGRA -> [("ada", MemPack.packByteString (adaOut @AllegraEra))]
  ERA_MARY -> [("ada", MemPack.packByteString (adaOut @MaryEra)), ("assets", MemPack.packByteString (L.mkBasicTxOut keyAddr (MaryValue (Coin 2000000) assets) :: L.TxOut MaryEra))]
  ERA_ALONZO -> [("ada", MemPack.packByteString (adaOut @AlonzoEra)), ("datum_hash", MemPack.packByteString (adaOut @AlonzoEra & dataHashTxOutL .~ SJust (unsafeMakeSafeHash (hashOf 0x33))))]
  ERA_BABBAGE -> [("ada", MemPack.packByteString (adaOut @BabbageEra)), ("datum_hash", MemPack.packByteString (adaOut @BabbageEra & dataHashTxOutL .~ SJust (unsafeMakeSafeHash (hashOf 0x33))))]
  ERA_DIJKSTRA -> [("ada", MemPack.packByteString (adaOut @DijkstraEra))]
  _ -> []
  where
    assets = MultiAsset (Map.fromList [(PolicyID (ScriptHash (hashOf 0x22)), Map.fromList [(AssetName (SBS.toShort (BS.pack [0x61, 0x61])), 5)])])

keyAddr :: Addr
keyAddr = Addr Mainnet (KeyHashObj (KeyHash (hashOf 0x11))) StakeRefNull

adaOut :: forall era. L.EraTxOut era => L.TxOut era
adaOut = L.mkBasicTxOut keyAddr (inject (Coin 1500000))

-- | blake2b-256 of the states the Go tests read from
-- cmd/dxnode-go/testdata/ledger_state/eras, which this module wrote.
nesDigests :: [(String, String)]
nesDigests =
  [ ("allegra_nes", "6c01ce0779d32300e603ff5f94832f52d832e4a3500db01f2506f032a08a8c96")
  , ("allegra_utxo_nes", "b58034ac0a0c79b820581cfbec540dd75527900b8c3101c9cf952e2bd0533306")
  , ("alonzo_nes", "2d8d512ef1dc808a64d5cb83072452bbf682bb2448badbce03e2b55bb54dffc1")
  , ("alonzo_utxo_nes", "107b3fd2a1de92dab2d31dcf829646c240a6b97a203760ae61b0f1e4f5fc4da9")
  , ("babbage_nes", "86091a4c5ba84f948d5d9931ba85ea094e422f882575075fe68e1d242a7e1b83")
  , ("babbage_utxo_nes", "a729f1c7730dab5058812d3e4907536b63b9c774169adde97a1e5a973df89381")
  , ("dijkstra_nes", "c0ca711ca381d19e96e377864f2ea5fd9ff87f309e95aad4c942f27aaf14fefd")
  , ("dijkstra_utxo_ix1_nes", "721e1471862bfcaccacd5e87aa70d5eccb10ecade5295639aa9591394edf8ec4")
  , ("dijkstra_utxo_nes", "b172cb4b0db3ac81cc952494c470850d814a147686cb8f8d794a4cd3657cbfb9")
  , ("mary_nes", "f44145f0aad0599a27095afdbff2ebc0386c1d9633500162a64c5bdc63fa4b14")
  , ("mary_utxo_nes", "5c453f511095564e7ed987123ca0e9ef393ba9dd996da41f75b492f22aa91814")
  , ("shelley_nes", "9bcaf14a81bd1ce1784cd1968e550f883f26e6eb0ef6f70904b39d66ce509296")
  , ("shelley_utxo_nes", "f061636f4889aa9ca976552a0b3ef941a457e722b7364374fdd7c2a3b168af7c")
  ]

eraNames :: [(String, Era, Word8)]
eraNames =
  [ ("shelley", ERA_SHELLEY, 2)
  , ("allegra", ERA_ALLEGRA, 3)
  , ("mary", ERA_MARY, 4)
  , ("alonzo", ERA_ALONZO, 5)
  , ("babbage", ERA_BABBAGE, 7)
  , ("dijkstra", ERA_DIJKSTRA, 12)
  ]

-- | A fixture by its file name.
nesNamed :: String -> Maybe BS.ByteString
nesNamed n = case [(e, ixs) | (name, e, _) <- eraNames, (suffix, ixs) <- [("_nes", []), ("_utxo_nes", [0]), ("_utxo_ix1_nes", [1])], name ++ suffix == n] of
  [(e, ixs)] -> nesFixture ixs e
  _ -> Nothing

nesCall :: Era -> BS.ByteString -> CodecLedgerStateResponse
nesCall e b = call (#codecLedgerState .~ (defMessage & #era .~ e & #format .~ LEDGER_STATE_FORMAT_NEW_EPOCH_STATE & #cbor .~ b)) ^. #codecLedgerState

-- | The views of the era states and MemPack rows, with the values the
-- builder 'sampleNes' put in.
eraStateChecks :: [Check]
eraStateChecks =
  [ ("era state: " ++ n ++ " is the committed fixture", maybe False (\b -> toHex (hashToBytes (hashWith @Blake2b_256 id b)) == d) (nesNamed n))
  | (n, d) <- nesDigests
  ]
    ++ concat
      [ [ (name ++ " state: epoch, pots and blocks made", epochOk (view e))
        , (name ++ " state: one pool", poolOk (view e))
        , (name ++ " state: mark snapshot and parameters", snapOk major (view e))
        , (name ++ " state: accounts", accountsOk (e /= ERA_DIJKSTRA) (view e))
        , (name ++ " state: no outputs", null (view e ^. #state . #utxo))
        , (name ++ " state: one output at index 0", outputOk (nesCall e (fromMaybe BS.empty (nesFixture [0] e))))
        , (name ++ " state: truncation is rejected", rejectedR (nesCall e (BS.init (fromMaybe BS.empty (nesFixture [] e))) ^. #rejection))
        ]
      | (name, e, major) <- eraNames
      , let view x = nesCall x (fromMaybe BS.empty (nesFixture [] x))
      ]
    ++ [ ( "ledger state: Byron is UNSUPPORTED"
         , call (#codecLedgerState .~ (defMessage & #era .~ ERA_BYRON & #format .~ LEDGER_STATE_FORMAT_MEMPACK_TXOUT & #cbor .~ BS.empty)) ^. #error . #code
             == HARNESS_ERROR_CODE_UNSUPPORTED
         )
       ]
    ++ [ ("mempack " ++ name ++ " " ++ label ++ ": re-encoded", mp e row == Just expected)
       | (name, e, label, row, expected) <- mempackRows
       ]
    ++ [ ( "divergence: a pinned-ledger " ++ name ++ " state decodes with the protocol version as a nested pair"
         , either (const False) (const True) (decodeStateOf e)
         )
       | (name, e, _) <- eraNames
       , e /= ERA_DIJKSTRA
       ]
    ++ [ ( "divergence: a UTxO key packs the output index little-endian"
         , case nesFixture [1] ERA_DIJKSTRA of
             Just b -> case decodeFull (eraProtVerLow @DijkstraEra) (BSL.fromStrict b) :: Either DecoderError (NewEpochState DijkstraEra) of
               Right nes -> Map.keys (unUTxO (utxosUtxo (lsUTxOState (esLState (nesEs nes))))) == [TxIn (TxId (unsafeMakeSafeHash (hashOf 0x42))) (TxIx 1)]
               Left _ -> False
             Nothing -> False
         )
       ]
  where
    rejectedR r = [(f ^. #rule, f ^. #name) | f <- r ^. #failures] == [("codec", "DeserialiseFailure")]
    pool = BS.replicate 28 0x77
    epochOk r =
      r ^. #epoch == 7
        && r ^. #state . #pots . #treasury == 1000
        && r ^. #state . #pots . #reserves == 2000
        && r ^. #state . #pots . #fees == 194490
        && r ^. #state . #pots . #donations == 0
        && map (\b -> (b ^. #poolId, b ^. #count)) (r ^. #blocksPrevious) == [(pool, 3)]
        && map (\b -> (b ^. #poolId, b ^. #count)) (r ^. #blocksCurrent) == [(pool, 5)]
    poolOk r = case r ^. #state . #pools of
      [p] -> p ^. #poolId == pool && p ^. #pledge == 71343432 && p ^. #cost == 400249652 && p ^. #deposit == 500000000
      _ -> False
    snapOk major r = case r ^. #snapshots of
      [e] -> e ^. #snapshot == S.SNAPSHOT_KIND_MARK && e ^. #amount == 40 && e ^. #poolId == pool && e ^. #credential . #hash == BS.replicate 28 0x01 && r ^. #pparams . #protocolVersion . #major == fromIntegral major
      _ -> False
    accountsOk expected r = case r ^. #state . #accounts of
      [a] -> expected && a ^. #deposit == 2000000 && a ^. #balance == 0 && a ^. #poolId == pool && a ^. #credential . #hash == BS.replicate 28 0x01
      [] -> not expected
      _ -> False
    outputOk r = case r ^. #state . #utxo of
      [u] -> u ^. #txin . #txId == BS.replicate 32 0x42 && u ^. #txin . #index == 0 && u ^. #txout == hex "82 581d" <> addr <> hex "1a 0016e360"
      _ -> False
    mp e row = let r = call (#codecLedgerState .~ (defMessage & #era .~ e & #format .~ LEDGER_STATE_FORMAT_MEMPACK_TXOUT & #cbor .~ row)) ^. #codecLedgerState in if null (r ^. #rejection . #failures) then Just (r ^. #cbor) else Nothing
    decodeStateOf :: Era -> Either DecoderError ()
    decodeStateOf e = case e of
      ERA_SHELLEY -> void' (dec @ShelleyEra)
      ERA_ALLEGRA -> void' (dec @AllegraEra)
      ERA_MARY -> void' (dec @MaryEra)
      ERA_ALONZO -> void' (dec @AlonzoEra)
      _ -> void' (dec @BabbageEra)
      where
        void' = fmap (const ())
        dec :: forall era. (L.Era era, DecCBOR (NewEpochState era)) => Either DecoderError (NewEpochState era)
        dec = decodeFull (eraProtVerLow @era) (BSL.fromStrict (fromMaybe BS.empty (nesFixture [] e)))
    mempackRows =
      [ (name, e, label, row, expected)
      | (name, e, _) <- eraNames
      , (label, row) <- mempackFixtures e
      , let expected = mempackExpected label
      ]
    mempackExpected label = case label of
      "assets" -> hex "82 581d" <> addr <> hex "82 1a001e8480 a1 581c" <> BS.replicate 28 0x22 <> hex "a1 42 6161 05"
      "datum_hash" -> hex "83 581d" <> addr <> hex "1a 0016e360 5820" <> h32
      _ -> hex "82 581d" <> addr <> hex "1a 0016e360"

tests :: [Check]
tests =
  [ ("tx: id, size and validity", txOk)
  , ("tx: IsValid false decodes as invalid", not (tx ERA_CONWAY (hex "84 a3 00 80 01 80 02 00 a0 f4 f6") ^. #codecTx . #isValid))
  , ("tx: trailing byte is rejected", rejected (tx ERA_CONWAY (BS.snoc minimalTx 0) ^. #codecTx . #rejection))
  , ("tx: truncation is rejected", rejected (tx ERA_CONWAY (BS.take 10 minimalTx) ^. #codecTx . #rejection))
  , ( "tx: protocol version outside the era is UNSUPPORTED"
    , call (#codecTx .~ (defMessage & #era .~ ERA_CONWAY & #protocolVersion . #major .~ 5 & #cbor .~ minimalTx)) ^. #error . #code
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "tx: protocol version 10 is inside Conway"
    , call (#codecTx .~ (defMessage & #era .~ ERA_CONWAY & #protocolVersion . #major .~ 10 & #cbor .~ minimalTx)) ^. #codecTx . #size == 10
    )
  , ("byron tx: id, size and validity", let r = byronTx byWire ^. #codecTx in r ^. #txId == hex "c103e30e691865a633cdbb41e53d5f00dcc79a4de12abc067547469a22e18562" && r ^. #size == fromIntegral (BS.length byWire) && r ^. #isValid)
  , ("byron tx: the encoding is rebuilt to the wire bytes", byronTx byWire ^. #codecTx . #cbor == byWire)
  , ("byron tx: trailing byte is rejected", rejected (byronTx (BS.snoc byWire 0) ^. #codecTx . #rejection))
  , ("byron tx: truncation is rejected", rejected (byronTx (BS.init byWire) ^. #codecTx . #rejection))
  , ("byron tx: an integer is no transaction", rejected (byronTx (hex "01") ^. #codecTx . #rejection))
  , ("byron tx: protocol version 2 is UNSUPPORTED", call (#codecTx .~ (defMessage & #era .~ ERA_BYRON & #protocolVersion . #major .~ 2 & #cbor .~ byWire)) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)
  , ("byron output: address and coin", let r = byronOut byOut ^. #codecTxOutput in r ^. #coin == 1000000 && r ^. #address == hex "82d818582183581cec0e23340c62b5e0d64ed01278769291f3ee83e721ea3375d8830d7ca0001aed599b89")
  , ("byron output: the encoding is rebuilt to the wire bytes", byronOut byOut ^. #codecTxOutput . #cbor == byOut)
  , ("byron output: truncation is rejected", rejected (byronOut (BS.init byOut) ^. #codecTxOutput . #rejection))
  , ("byron output: trailing byte is rejected", rejected (byronOut (BS.snoc byOut 0) ^. #codecTxOutput . #rejection))
  , ("byron output: an integer is no output", rejected (byronOut (hex "01") ^. #codecTxOutput . #rejection))
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
  , ( "hfc block: a Byron regular block is tag 1"
    , let r = hfc 0 byronDisk ^. #codecHfcBlock
       in r ^. #era == ERA_BYRON
            && toHex (r ^. #headerHash) == "1451a0dbf16cfeddf4991a838961df1b08a68f43a19c0eb3b36cc4029c77a2d8"
            && r ^. #cbor == byronDisk
    )
  , ( "hfc block: node-to-node form is the disk form in tag 24"
    , let r = hfc 14 (wrap24 byronDisk) ^. #codecHfcBlock
       in r ^. #era == ERA_BYRON && r ^. #cbor == wrap24 byronDisk
    )
  , ("hfc block: a regular block under the boundary tag is rejected", rejected (hfc 0 (hex "82 00" <> hex byronBlock) ^. #codecHfcBlock . #rejection))
  , ("hfc block: tag 9 is rejected", rejected (hfc 0 (hex "82 09" <> hex byronBlock) ^. #codecHfcBlock . #rejection))
  , ("hfc block: a Byron block under tag 7 is rejected", rejected (hfc 0 (hex "82 07" <> hex byronBlock) ^. #codecHfcBlock . #rejection))
  , ("hfc block: trailing byte is rejected", rejected (hfc 0 (BS.snoc byronDisk 0) ^. #codecHfcBlock . #rejection))
  , ("hfc block: truncation is rejected", rejected (hfc 0 (BS.init byronDisk) ^. #codecHfcBlock . #rejection))
  , ( "hfc block: a Dijkstra block is tag 8"
    , let r = hfc 0 djBlock ^. #codecHfcBlock
       in r ^. #era == ERA_DIJKSTRA && toHex (r ^. #headerHash) == djHeaderHash && r ^. #cbor == djBlock
    )
  , ( "hfc block: a Dijkstra block in node-to-node form"
    , let r = hfc 14 (wrap24 djBlock) ^. #codecHfcBlock
       in r ^. #era == ERA_DIJKSTRA && toHex (r ^. #headerHash) == djHeaderHash && r ^. #cbor == wrap24 djBlock
    )
  , ("hfc block: an empty payload under tag 8 is rejected", rejected (hfc 0 (hex "82 08 80") ^. #codecHfcBlock . #rejection))
  , ("hfc block: junk under tag 8 is rejected", rejected (hfc 0 (hex "82 08 ff") ^. #codecHfcBlock . #rejection))
  , ("hfc block: a Dijkstra block cut by one byte is rejected", rejected (hfc 0 (BS.init djBlock) ^. #codecHfcBlock . #rejection))
  , ( "hfc block: the consensus golden Dijkstra block is rejected for its empty invalid-transaction set"
    , let r = hfc 0 djGolden ^. #codecHfcBlock . #rejection
       in rejected r && any (T.isInfixOf "Expected a non-empty set" . (^. #detail)) (r ^. #failures)
    )
  , ( "divergence: a Dijkstra block body has four components, the first the invalid-transaction set"
    , either (const False) (const True) (djBody (hex "84 f6 80 f6 f6"))
    )
  , ( "divergence: a Dijkstra block body of three components is rejected"
    , either (const True) (const False) (djBody (hex "83 80 f6 f6"))
    )
  , ("hfc block: node-to-node form without tag 24 is rejected", rejected (hfc 14 byronDisk ^. #codecHfcBlock . #rejection))
  , ("ledger state: epoch and pots", let r = nes ^. #codecLedgerState in r ^. #epoch == 7 && (r ^. #state . #pots . #treasury, r ^. #state . #pots . #reserves, r ^. #state . #pots . #fees) == (1000, 2000, 194490))
  , ( "ledger state: accounts"
    , let as = nes ^. #codecLedgerState . #state . #accounts
       in length as == 3
            && toHex (head as ^. #credential . #hash) == "1601bb34497f96e8dc7abfbffafdc4ec8b71b87472593c41403097e1"
            && (head as ^. #balance, head as ^. #deposit) == (500000000, 2000000)
            && toHex (as !! 2 ^. #poolId) == "6a04213acc095f128dc4d90bd88b34a3b0c5c7dc2accd1e97ab9fe96"
            && fmap (toHex . (^. #hash)) (as !! 2 ^. #maybe'drep >>= (^. #maybe'credential)) == Just "ab0a0fd54097b56f33e1ad7e3204ac57fd80e99db43bb98734b875ca"
    )
  , ( "ledger state: current and future pool"
    , let ps = nes ^. #codecLedgerState . #state . #pools
       in map (^. #stage) ps == [S.POOL_STAGE_CURRENT, S.POOL_STAGE_FUTURE]
            && toHex (head ps ^. #vrfKeyHash) == "95f688dae2d137bd97cd47326225f73e643c705d526b146d08a0a80fed70c080"
            && (head ps ^. #pledge, head ps ^. #cost, head ps ^. #deposit) == (71343432, 400249652, 500000000)
            && toHex (ps !! 1 ^. #vrfKeyHash) == "e2f2beb2f08c53ec688bebf83d6712461f2cfb695eba187c9e91277029559225"
            && (ps !! 1 ^. #deposit) == 500000000
    )
  , ( "ledger state: DRep and committee"
    , let st = nes ^. #codecLedgerState . #state
       in map (^. #deposit) (st ^. #dreps) == [70000000]
            && length (st ^. #committee . #members) == 2
            && st ^. #committee . #threshold . #numerator == "1"
    )
  , ( "ledger state: parameters"
    , let p = nes ^. #codecLedgerState . #pparams in (p ^. #maybe'minFeeA, p ^. #maybe'keyDeposit, p ^. #protocolVersion . #major) == (Just 44, Just 2000000, 10)
    )
  , ( "ledger state: blocks and snapshots"
    , let r = nes ^. #codecLedgerState
       in map (^. #count) (r ^. #blocksCurrent) == [5]
            && map (^. #count) (r ^. #blocksPrevious) == [3]
            && map (\e -> (e ^. #snapshot, e ^. #amount)) (r ^. #snapshots) == [(S.SNAPSHOT_KIND_MARK, 40)]
    )
  , ("ledger state: truncation is rejected", rejected (lstate LEDGER_STATE_FORMAT_NEW_EPOCH_STATE (BS.init newEpochState) ^. #codecLedgerState . #rejection))
  , ("divergence: a trailing byte after a NewEpochState is rejected", rejected (lstate LEDGER_STATE_FORMAT_NEW_EPOCH_STATE (BS.snoc newEpochState 0) ^. #codecLedgerState . #rejection))
  , ("ledger state: an integer is no state", rejected (lstate LEDGER_STATE_FORMAT_NEW_EPOCH_STATE (hex "01") ^. #codecLedgerState . #rejection))
  , ("divergence: a six-element array is no NewEpochState", rejected (lstate LEDGER_STATE_FORMAT_NEW_EPOCH_STATE (BS.cons 0x86 (BS.tail newEpochState)) ^. #codecLedgerState . #rejection))
  , ( "ledger state: Byron is UNSUPPORTED"
    , call (#codecLedgerState .~ (defMessage & #era .~ ERA_BYRON & #format .~ LEDGER_STATE_FORMAT_NEW_EPOCH_STATE & #cbor .~ newEpochState)) ^. #error . #code
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ("ledger state: unspecified format is BAD_REQUEST", call (#codecLedgerState .~ (defMessage & #era .~ ERA_CONWAY)) ^. #error . #code == HARNESS_ERROR_CODE_BAD_REQUEST)
  , ( "divergence: a DRep keeps its stored expiry, the dormant epochs are not added"
    , map (^. #expiry) (nes ^. #codecLedgerState . #state . #dreps) == [999]
    )
  , ("divergence: a MemPack ada-only output is written as the two-element array", mempack mempackAda ^. #codecLedgerState . #cbor == hex "82 581d" <> addr <> hex "1a 000f4240")
  , ( "divergence: a key and a script credential with one hash stay two snapshot entries"
    , either (const False) (\ss -> VMap.size (unActiveStake (ssActiveStake (ssStakeMark ss))) == 2) (decodeFull (eraProtVerLow @ConwayEra) (BSL.fromStrict sharedSnapshots) :: Either DecoderError (SnapShots ConwayEra))
    )
  , ("mempack: truncation is rejected", rejected (mempack (BS.init mempackAda) ^. #codecLedgerState . #rejection))
  , ("mempack: a trailing byte is rejected", rejected (mempack (BS.snoc mempackAda 0) ^. #codecLedgerState . #rejection))
  , ("mempack: tag 9 is rejected", rejected (mempack (BS.cons 9 (BS.tail mempackAda)) ^. #codecLedgerState . #rejection))
  , ("mempack: empty input is rejected", rejected (mempack BS.empty ^. #codecLedgerState . #rejection))
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
  , ( "dijkstra tx: id, size and validity"
    , let r = tx ERA_DIJKSTRA djTx ^. #codecTx
       in r ^. #txId == hashToBytes (hashWith @Blake2b_256 id (hex "a3 00 80 01 80 02 00")) && r ^. #size == 10 && r ^. #isValid && r ^. #cbor == djTx
    )
  , ("dijkstra tx: trailing byte is rejected", rejected (tx ERA_DIJKSTRA (BS.snoc djTx 0) ^. #codecTx . #rejection))
  , ("dijkstra tx: truncation is rejected", rejected (tx ERA_DIJKSTRA (BS.take 9 djTx) ^. #codecTx . #rejection))
  , ("dijkstra tx: an integer is no transaction", rejected (tx ERA_DIJKSTRA (hex "01") ^. #codecTx . #rejection))
  , ( "dijkstra tx: only protocol version 12 is inside the era"
    , map (\m -> (call (#codecTx .~ (defMessage & #era .~ ERA_DIJKSTRA & #protocolVersion . #major .~ m & #cbor .~ djTx)) ^. #error . #code == HARNESS_ERROR_CODE_UNSUPPORTED)) [11, 12, 13]
        == [True, False, True]
    )
  , ( "tx: Conway stops at protocol version 11"
    , call (#codecTx .~ (defMessage & #era .~ ERA_CONWAY & #protocolVersion . #major .~ 12 & #cbor .~ minimalTx)) ^. #error . #code
        == HARNESS_ERROR_CODE_UNSUPPORTED
    )
  , ( "dijkstra output: datum hash"
    , let r = output ERA_DIJKSTRA (hex "a3 00 581d" <> addr <> hex "01 1a0016e360 02 82 00 5820" <> h32) ^. #codecTxOutput
       in r ^. #datumHash == h32 && r ^. #coin == 1500000 && r ^. #address == addr
    )
  , ("dijkstra output: an integer is no output", rejected (output ERA_DIJKSTRA (hex "01") ^. #codecTxOutput . #rejection))
  , ("dijkstra certificate: tag 7 is RegDepositTxCert", kindOf ERA_DIJKSTRA (hex "83 07" <> key <> hex "1a 001e8480") == "RegDepositTxCert")
  , ("dijkstra certificate: tag 8 is UnRegDepositTxCert", kindOf ERA_DIJKSTRA (hex "83 08" <> key <> hex "1a 001e8480") == "UnRegDepositTxCert")
  , ("dijkstra certificate: vote delegation is DelegTxCert", kindOf ERA_DIJKSTRA (hex "83 09" <> key <> hex "81 02") == "DelegTxCert")
  , ( "dijkstra governance: info proposal round-trips"
    , let r = djGov GOVERNANCE_ITEM_KIND_PROPOSAL proposalInfo ^. #codecGovernance
       in r ^. #actionType == GOV_ACTION_TYPE_INFO && r ^. #cbor == proposalInfo
    )
  , ("dijkstra governance: voting procedures round-trip", djGov GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES votes ^. #codecGovernance . #cbor == votes)
  , ("dijkstra governance: trailing byte is rejected", rejected (djGov GOVERNANCE_ITEM_KIND_VOTING_PROCEDURES (BS.snoc votes 0) ^. #codecGovernance . #rejection))
  , ("dijkstra header: hash, slot and block number", djHeaderOk)
  , ("dijkstra header: truncation is rejected", rejected (header ERA_DIJKSTRA (BS.init djHeaderBytes) ^. #codecBlockHeader . #rejection))
  , ("dijkstra header: trailing byte is rejected", rejected (header ERA_DIJKSTRA (BS.snoc djHeaderBytes 0) ^. #codecBlockHeader . #rejection))
  , ( "dijkstra block body: no transactions and the header's body hash"
    , let r = call (#codecBlockBody .~ (defMessage & #era .~ ERA_DIJKSTRA & #cbor .~ BS.drop 2 djBlock)) ^. #codecBlockBody
       in null (r ^. #rejection . #failures) && r ^. #txCount == 0 && BS.length (r ^. #bodyHash) == 32
    )
  , ( "dijkstra block body: truncation is rejected"
    , rejected (call (#codecBlockBody .~ (defMessage & #era .~ ERA_DIJKSTRA & #cbor .~ BS.init (BS.drop 2 djBlock))) ^. #codecBlockBody . #rejection)
    )
  , ( "dijkstra pparams update: max tx size"
    , let r = ppu ERA_DIJKSTRA "a1 03 19 4000" ^. #codecPparamsUpdate in r ^. #update . #maybe'maxTxSize == Just 16384 && r ^. #cbor == hex "a1 03 19 4000"
    )
  , ( "dijkstra pparams update: committee size and action lifetime"
    , let u = ppu ERA_DIJKSTRA "a2 18 1b 07 18 1d 06" ^. #codecPparamsUpdate . #update
       in u ^. #maybe'committeeMinSize == Just 7 && u ^. #maybe'govActionLifetime == Just 6
    )
  , ("dijkstra pparams update: trailing byte is rejected", rejected (ppu ERA_DIJKSTRA "a1 03 19 4000 00" ^. #codecPparamsUpdate . #rejection))
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
  , ( "divergence: Dijkstra rejects the expunged deposit-less registration, tag 0"
    , either (const True) (const False) (djCert (hex "82 00" <> key))
    )
  , ( "divergence: Dijkstra rejects the expunged deposit-less deregistration, tag 1"
    , either (const True) (const False) (djCert (hex "82 01" <> key))
    )
  , ( "divergence: Dijkstra rejects genesis delegation, tag 5"
    , either (const True) (const False) (djCert (hex "84 05 581c" <> h28 <> hex "581c" <> BS.replicate 28 0x22 <> hex "5820" <> h32))
    )
  , ( "divergence: a Dijkstra mempool transaction is re-encoded without its is_valid flag"
    , reencodeDjTx (hex "84 a3 00 80 01 80 02 00 a0 f5 f6") == Right djTx
    )
  , ( "divergence: a Dijkstra output with an empty multi-asset is rejected"
    , either (const True) (const False) (reencodeDjOut (hex "82 581d" <> addr <> hex "82 1a1a1e8528 a0"))
        && reencodeDjOut (hex "82 581d" <> addr <> hex "1a 1a1e8528") == Right (hex "82 581d" <> addr <> hex "1a 1a1e8528")
    )
  , ( "divergence: a Dijkstra map output with only a datum hash is re-encoded as a legacy array"
    , reencodeDjOut (hex "a3 00 581d" <> addr <> hex "01 1a0016e360 02 82 00 5820" <> h32)
        == Right (hex "83 581d" <> addr <> hex "1a0016e360 5820" <> h32)
    )
  , ( "divergence: a Byron transaction with definite-length input and output lists is rejected"
    , either (const True) (const False) (decodeByronTx byDefinite) && either (const False) (const True) (decodeByronTx byWire)
    )
  , ( "divergence: a three-element Conway transaction is accepted and re-encoded with IsValid"
    , reencodeTx (hex "83 a3 00 80 01 80 02 00 a0 f6") == Right (hex "84 a3 00 80 01 80 02 00 a0 f5 f6")
    )
  ]
    ++ eraStateChecks
  where
    ppu e h = call (#codecPparamsUpdate .~ (defMessage & #era .~ e & #cbor .~ hex h))
    tx e b = call (#codecTx .~ (defMessage & #era .~ e & #cbor .~ b))
    output e b = call (#codecTxOutput .~ (defMessage & #era .~ e & #cbor .~ b))
    cert e b = call (#codecCertificate .~ (defMessage & #era .~ e & #cbor .~ b))
    header e b = call (#codecBlockHeader .~ (defMessage & #era .~ e & #cbor .~ b))
    lstate f b = call (#codecLedgerState .~ (defMessage & #era .~ ERA_CONWAY & #format .~ f & #cbor .~ b))
    sharedSnapshots =
      let entry = hex "82 00 581c" <> h28 <> hex "82 18 28 581c" <> h28 <> hex "82 01 581c" <> h28 <> hex "82 18 28 581c" <> h28
          snap = hex "82 a2" <> entry <> hex "a0"
       in hex "84" <> snap <> snap <> snap <> hex "00"
    mempack = lstate LEDGER_STATE_FORMAT_MEMPACK_TXOUT
    mempackAda = hex "00 1d" <> addr <> hex "00 bd 84 40"
    newEpochState = hex newEpochStateHex
    nes = lstate LEDGER_STATE_FORMAT_NEW_EPOCH_STATE newEpochState
    hfc n b = call (#codecHfcBlock .~ (defMessage & #nodeToNodeVersion .~ n & #cbor .~ b))
    byronDisk = hex "82 01" <> hex byronBlock
    djHeader = hex "828a03095820bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa58208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c58208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c8258404fefc7c718693b57c87170ceba220382afbdd148c0a53b4a009ca63ad1f101483a6170c83a77f23d362a68dcb502802df7f98fa4f7a78b4082b211530e1234305850f770f6769ae9871d42b970fc6254bb927c2181fff45897f241bd72221d86d33c8df64c0a3c8cbb9aa52fef191d7202465c52df8d33727a38c7dc5d40864d753348a340f8afcbb3bb05d4a03f16b1080d190929582029571d16f081709b3c48651860077bebf9340abb3fc7133443c54f1f5a5edcf1845820d298da3803eb9958f01c02e73f2410f2f9bb2ecbc346526b1b76772e1bdd7db500005840940f8a3696847e4a238705bdd27a345086282067b9bc5cb7b69847ca8756085844d576f59ab056c169a504320cc1eab4c11fd529482b3c57da6fa96d44635b088202005901c0554d4f7d2acf9499b3a0cfc6089648863e19ff47f39a6d201923d39e76cd968417b2670dd7e942063f8463df9531bbef628040390c02c45e4922a9f0acafb00fcc6a5ab5c9b40f817c715df558af7d07b6186f0ccf31715ec2fb00980730ac166af657e6670608afe1bf651d496e01b1c7ff1eb44614d8cfd1b7e32b2c2939349236cc0ada145d8d8d7ad919ef1e60c8bbad31dbedf9f395849705a00c14a8785106aae31f55abc5b1f2089cbef16d9401f158704c1e4f740f7125cfc700a99d97d0332eacb33e4bbc8dab2872ec2b3df9e113addaebd156bfc64fdfc732614d2aedd10a58a34993b7b08c822af3aa615b6bbb9b267bc902e4f1075e194aed084ca18f8bcde1a6b094bf3f5295a0d454c0a083ed5b74f7092fc0a7346c03979a30eeea76d686e512ba48d21544ba874886cdd166cbf275b11f1f3881f4c4277c09a24b88fc6168f4578267bdc9d62cb9b78b8dfc888ccce226a177725f39e7d50930552861d1e88b7898971c780dc3b773321ba1854422b5cecead7d50e77783050eeae2cd9595b9cd91681c72e5d53bb7d12f28dec9b2847ee70a3d7781fb1133aea3b169f536ff5945ec0a76950e51beded0627bb78120617a2f0842e50e39"
    -- The header of the consensus golden Dijkstra block under the body the
    -- pinned ledger writes: [invalid transactions, transactions, leios, peras].
    djBlock = hex "82 08 82" <> djHeader <> hex "84 f6 80 f6 f6"
    -- The golden's own body is the older layout: transaction bodies,
    -- witnesses, auxiliary data, invalid transactions.
    djGolden = hex "82 08 86" <> djHeader <> hex "80 80 a0 80"
    byWire = hex "82839f8200d8185824825820090000000000000000000000000000000000000000000000000000000000000001ff9f8282d818582183581cec0e23340c62b5e0d64ed01278769291f3ee83e721ea3375d8830d7ca0001aed599b891a000f4240ffa0818200d81858858258408a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b584044cfdbee4dcbc79b988909b6099a607aaaf07063205ad458980b31ff6de5c59335e022c678690aaee9593522509343f890e3db1d4d309762160e94381a31b007"
    byOut = hex "8282d818582183581cec0e23340c62b5e0d64ed01278769291f3ee83e721ea3375d8830d7ca0001aed599b891a000f4240"
    byInput = BS.take 42 (BS.drop 3 byWire)
    byWitnesses = BS.drop (3 + 42 + 1 + 1 + BS.length byOut + 1 + 1) byWire
    byDefinite = hex "82 83 81" <> byInput <> hex "81" <> byOut <> hex "a0" <> byWitnesses
    decodeByronTx :: BS.ByteString -> Either DecoderError (ByronUTxO.ATxAux BS.ByteString)
    decodeByronTx = decodeFullAnnotatedBytes byronProtVer "ATxAux" decCBOR . BSL.fromStrict
    byronTx b = call (#codecTx .~ (defMessage & #era .~ ERA_BYRON & #cbor .~ b))
    byronOut b = call (#codecTxOutput .~ (defMessage & #era .~ ERA_BYRON & #cbor .~ b))
    djTx = hex "83 a3 00 80 01 80 02 00 a0 f6"
    djHeaderBytes = djHeader
    djGov k b = call (#codecGovernance .~ (defMessage & #era .~ ERA_DIJKSTRA & #kind .~ k & #cbor .~ b))
    djHeaderOk =
      let r = header ERA_DIJKSTRA djHeaderBytes ^. #codecBlockHeader
       in toHex (r ^. #headerHash) == djHeaderHash && r ^. #slot == 9 && r ^. #blockNo == 3 && r ^. #cbor == djHeaderBytes
    djHeaderHash = "1a68d6908bdfec726530a7da46bdda2a459e12ea386679e3807dff01d2e1ecb3"
    djBody :: BS.ByteString -> Either DecoderError (DijkstraBlockBody DijkstraEra)
    djBody = decodeFullAnnotator (eraProtVerLow @DijkstraEra) "block body" decCBOR . BSL.fromStrict
    wrap24 b = hex "d8 18" <> bytesHeader (BS.length b) <> b
    bytesHeader n
      | n < 24 = BS.pack [0x40 + fromIntegral n]
      | n < 256 = BS.pack [0x58, fromIntegral n]
      | n < 65536 = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
      | otherwise = BS.pack [0x5a, fromIntegral (n `div` 16777216), fromIntegral (n `div` 65536 `mod` 256), fromIntegral (n `div` 256 `mod` 256), fromIntegral (n `mod` 256)]
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
    djCert :: BS.ByteString -> Either DecoderError (TxCert DijkstraEra)
    djCert = decodeFull (eraProtVerLow @DijkstraEra) . BSL.fromStrict
    reencodeDjTx :: BS.ByteString -> Either DecoderError BS.ByteString
    reencodeDjTx b = serialize' dv <$> (decodeFullAnnotator dv "Tx" decCBOR (BSL.fromStrict b) :: Either DecoderError (Tx TopTx DijkstraEra))
    reencodeDjOut :: BS.ByteString -> Either DecoderError BS.ByteString
    reencodeDjOut b = serialize' dv <$> (decodeFull dv (BSL.fromStrict b) :: Either DecoderError (TxOut DijkstraEra))
    dv = eraProtVerLow @DijkstraEra

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

newEpochStateHex :: String
newEpochStateHex =
  concat
    [ "8707a1581c7777777777777777777777777777777777777777777777777777777703a1581c77777777777777777777777777"
    , "7777777777777777777777777777770584821903e81907d0828383a18200581cab0a0fd54097b56f33e1ad7e3204ac57fd80"
    , "e99db43bb98734b875ca841903e7801a042c1d80d90102818200581cc74dcd423bf991e50eb9956086a0496ffc7d44c5079f"
    , "9df0f5931cffa00584a1582095f688dae2d137bd97cd47326225f73e643c705d526b146d08a0a80fed70c08001a1581c6a04"
    , "213acc095f128dc4d90bd88b34a3b0c5c7dc2accd1e97ab9fe968a582095f688dae2d137bd97cd47326225f73e643c705d52"
    , "6b146d08a0a80fed70c0801a04409d481a17db5334d81e8200018200581c9e4766b9d46c0d452d3913417cf07fb7f8831115"
    , "d2f0235f6ce20cb8d901028080801a1dcd6500d90102818200581cc74dcd423bf991e50eb9956086a0496ffc7d44c5079f9d"
    , "f0f5931cffa1581c6a04213acc095f128dc4d90bd88b34a3b0c5c7dc2accd1e97ab9fe9689581c6a04213acc095f128dc4d9"
    , "0bd88b34a3b0c5c7dc2accd1e97ab9fe965820e2f2beb2f08c53ec688bebf83d6712461f2cfb695eba187c9e912770295592"
    , "251a03292ce91a168a4344d81e820001581de09e4766b9d46c0d452d3913417cf07fb7f8831115d2f0235f6ce20cb8d90102"
    , "8080f6a084a38200581c1601bb34497f96e8dc7abfbffafdc4ec8b71b87472593c41403097e1841a1dcd65001a001e8480f6"
    , "f68200581c9e4766b9d46c0d452d3913417cf07fb7f8831115d2f0235f6ce20cb884001a001e8480f6f68200581cc74dcd42"
    , "3bf991e50eb9956086a0496ffc7d44c5079f9df0f5931cff84001a001e8480581c6a04213acc095f128dc4d90bd88b34a3b0"
    , "c5c7dc2accd1e97ab9fe968200581cab0a0fd54097b56f33e1ad7e3204ac57fd80e99db43bb98734b875caa0a084a0a00000"
    , "86a15822bc9a69cf21215cd42c54a088aa15318a6a3eb68f53a16126874983e7b18e20f1000058470201c489488f55eea3db"
    , "7a3c3441b57cb8c57518e99323fdd00f9ec494c2868d27e29ee0ede67d2b095a8a94e42d918b6e0fe49fdc71010000000b04"
    , "f74600cff7e8abb5899b1a1a225510001a0002f7ba87828480808080808182a28200581c204c5f1bafe8ee289c881cb48d5f"
    , "1b2abf97e8720d18526561d93be71903938200581c65d36c561e076d2a6fe08172619d48bdcfd9056e4ee79cbf2cfcbccb19"
    , "0393d81e8201018282782368747470733a2f2f63617264616e6f2d636f6e737469747574696f6e2e63727970746f5820e5f5"
    , "f212e67762d5fc4727a8bf9cdfaaf9dc0ca8e0f5cf51f32a214a5242cd44581cfa24fb305126805cf2164c161d852a0e7330"
    , "cf988f1fe558cf7d4a64981f182c1a00025ef51a00010000194000001a001e84801a1dcd6500121896d81e82030ad81e8203"
    , "1903e8d81e820105820a001a1443fd001910d6a3009f1a000189b41901a401011903e818ad00011903e819ea350401192baf"
    , "18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e801864"
    , "1a000170a718201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a0003"
    , "7c71187a0001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb201"
    , "1a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147184a0119a9151902"
    , "280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a00"
    , "037c71187a0001011a0001614219020700011a000122c118201a00014f581a00037c71187a0001011a00014f581a00037c71"
    , "187a0001011a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013e"
    , "ff182019e86a1820194eae182019600c1820195108182019654d182019602f18201a032e93af1937fd0aff019f1a000189b4"
    , "1901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864193e801864193e80186419"
    , "3e801864193e801864193e80186418641864193e8018641a000170a718201a00020782182019f016041a0001194a18b20001"
    , "19568718201a0001643519030104021a00014f581a00037c71187a0001011903e819a7a90402195fe419733a1826011a000d"
    , "b464196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a00012911011933"
    , "71041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa18"
    , "20191c4b1820191cdf1820192d1a18201a00014f581a00037c71187a0001011a0001614219020700011a000122c118201a00"
    , "014f581a00037c71187a0001011a00014f581a00037c71187a0001011a000e94721a0003414000021a0004213c19583c041a"
    , "00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae18201960"
    , "0c1820195108182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a1a0013be0419702c"
    , "183f00011a000f59d919aa6718fb0001ff029f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a"
    , "000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a0001"
    , "70a718201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a0001e14319"
    , "1c893903831906b419022518391a00014f580001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f"
    , "19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147"
    , "184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf"
    , "1820192d1a18201a00014f581a0001e143191c893903831906b419022518391a00014f5800011a0001614219020700011a00"
    , "0122c118201a00014f581a0001e143191c893903831906b419022518391a00014f580001011a00014f581a0001e143191c89"
    , "3903831906b419022518391a00014f5800011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194f"
    , "f30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c182019510818201965"
    , "4d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e8018641a000eaf1f121a00"
    , "2a6e06061a0006be98011a0321aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a001e743c18241a0031"
    , "410f0c1a000dbf9e011a09f2f6d31910d318241a0004578218241a096e44021967b518241a0473cee818241a13e62472011a"
    , "0f23d40118481a00212c5618481a0022814619fc3b041a00032b00192076041a0013be0419702c183f00011a000f59d919aa"
    , "6718fb00011a000187551902d61902cf00011a000187551902d61902cf00011a000187551902d61902cf00011a0001a56619"
    , "02a800011a00017468011a00044a391949a000011a0002bfe2189f01011a00026b371922ee00011a00026e9219226d00011a"
    , "0001a3e2190ce2011a00019e4919028f011a001df8bb195fc803ff82d81e82190241192710d81e821902d11a00989680821a"
    , "009896801b00000002540be400821a02faf0801b00000009502f900019138818960385d81e8218331864d81e820d14d81e82"
    , "0d14d81e8218331864d81e82183318648ad81e8218331864d81e820d14d81e820d14d81e820d14d81e8218331864d81e8218"
    , "331864d81e8218331864d81e8218331864d81e820304d81e82183318640114181e187b1a042c1d801864d81e820f01981f18"
    , "2c1a00025ef51a00010000194000001a001e84801a1dcd6500121896d81e82030ad81e82031903e8d81e820105820a001a14"
    , "43fd001910d6a3009f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e"
    , "801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a0002078218"
    , "2019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a00037c71187a0001011903e819a7a9"
    , "0402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce18"
    , "20193bc318201a0001291101193371041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18"
    , "201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a00037c71187a0001011a00016142"
    , "19020700011a000122c118201a00014f581a00037c71187a0001011a00014f581a00037c71187a0001011a0004213c19583c"
    , "041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae1820"
    , "19600c1820195108182019654d182019602f18201a032e93af1937fd0aff019f1a000189b41901a401011903e818ad000119"
    , "03e819ea350401192baf18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e8018"
    , "6418641864193e8018641a000170a718201a00020782182019f016041a0001194a18b2000119568718201a00016435190301"
    , "04021a00014f581a00037c71187a0001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e01"
    , "1999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a0119"
    , "7147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d"
    , "1a18201a00014f581a00037c71187a0001011a0001614219020700011a000122c118201a00014f581a00037c71187a000101"
    , "1a00014f581a00037c71187a0001011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194ff30104"
    , "001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d1820"
    , "19602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a1a0013be0419702c183f00011a000f59d919aa6718"
    , "fb0001ff029f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864"
    , "193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a00020782182019f0"
    , "16041a0001194a18b2000119568718201a0001643519030104021a00014f581a0001e143191c893903831906b41902251839"
    , "1a00014f580001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2"
    , "011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147184a0119a91519"
    , "02280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a"
    , "0001e143191c893903831906b419022518391a00014f5800011a0001614219020700011a000122c118201a00014f581a0001"
    , "e143191c893903831906b419022518391a00014f580001011a00014f581a0001e143191c893903831906b419022518391a00"
    , "014f5800011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a00"
    , "0189b41901a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d182019602f18201a0290f1e7"
    , "0a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e8018641a000eaf1f121a002a6e06061a0006be98011a0321"
    , "aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a001e743c18241a0031410f0c1a000dbf9e011a09f2f6"
    , "d31910d318241a0004578218241a096e44021967b518241a0473cee818241a13e62472011a0f23d40118481a00212c561848"
    , "1a0022814619fc3b041a00032b00192076041a0013be0419702c183f00011a000f59d919aa6718fb00011a000187551902d6"
    , "1902cf00011a000187551902d61902cf00011a000187551902d61902cf00011a0001a5661902a800011a00017468011a0004"
    , "4a391949a000011a0002bfe2189f01011a00026b371922ee00011a00026e9219226d00011a0001a3e2190ce2011a00019e49"
    , "19028f011a001df8bb195fc803ff82d81e82190241192710d81e821902d11a00989680821a009896801b00000002540be400"
    , "821a02faf0801b00000009502f900019138818960385d81e8218331864d81e820d14d81e820d14d81e8218331864d81e8218"
    , "3318648ad81e8218331864d81e820d14d81e820d14d81e820d14d81e8218331864d81e8218331864d81e8218331864d81e82"
    , "18331864d81e820304d81e82183318640114181e187b1a042c1d801864d81e820f01820280828480a18200581cab0a0fd540"
    , "97b56f33e1ad7e3204ac57fd80e99db43bb98734b875ca00a18200581cab0a0fd54097b56f33e1ad7e3204ac57fd80e99db4"
    , "3bb98734b875ca841903e7801a042c1d80d90102818200581cc74dcd423bf991e50eb9956086a0496ffc7d44c5079f9df0f5"
    , "931cffa1581c6a04213acc095f128dc4d90bd88b34a3b0c5c7dc2accd1e97ab9fe960084878182a28200581c204c5f1bafe8"
    , "ee289c881cb48d5f1b2abf97e8720d18526561d93be71903938200581c65d36c561e076d2a6fe08172619d48bdcfd9056e4e"
    , "e79cbf2cfcbccb190393d81e8201018282782368747470733a2f2f63617264616e6f2d636f6e737469747574696f6e2e6372"
    , "7970746f5820e5f5f212e67762d5fc4727a8bf9cdfaaf9dc0ca8e0f5cf51f32a214a5242cd44581cfa24fb305126805cf216"
    , "4c161d852a0e7330cf988f1fe558cf7d4a64981f182c1a00025ef51a00010000194000001a001e84801a1dcd6500121896d8"
    , "1e82030ad81e82031903e8d81e820105820a001a1443fd001910d6a3009f1a000189b41901a401011903e818ad00011903e8"
    , "19ea350401192baf18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418"
    , "641864193e8018641a000170a718201a00020782182019f016041a0001194a18b2000119568718201a000164351903010402"
    , "1a00014f581a00037c71187a0001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999"
    , "101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147"
    , "184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18"
    , "201a00014f581a00037c71187a0001011a0001614219020700011a000122c118201a00014f581a00037c71187a0001011a00"
    , "014f581a00037c71187a0001011a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b419"
    , "01a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d182019602f18201a032e93af1937fd0a"
    , "ff019f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864193e80"
    , "1864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a00020782182019f016041a"
    , "0001194a18b2000119568718201a0001643519030104021a00014f581a00037c71187a0001011903e819a7a90402195fe419"
    , "733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc31820"
    , "1a0001291101193371041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a96"
    , "18201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a00037c71187a0001011a0001614219020700011a"
    , "000122c118201a00014f581a00037c71187a0001011a00014f581a00037c71187a0001011a000e94721a0003414000021a00"
    , "04213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a18"
    , "20194eae182019600c1820195108182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a"
    , "1a0013be0419702c183f00011a000f59d919aa6718fb0001ff029f1a000189b41901a401011903e818ad00011903e819ea35"
    , "0401192baf18201a000312591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864"
    , "193e8018641a000170a718201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a0001"
    , "4f581a0001e143191c893903831906b419022518391a00014f580001011903e819a7a90402195fe419733a1826011a000db4"
    , "64196a8f0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371"
    , "041956540a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820"
    , "191c4b1820191cdf1820192d1a18201a00014f581a0001e143191c893903831906b419022518391a00014f5800011a000161"
    , "4219020700011a000122c118201a00014f581a0001e143191c893903831906b419022518391a00014f580001011a00014f58"
    , "1a0001e143191c893903831906b419022518391a00014f5800011a000e94721a0003414000021a0004213c19583c041a0016"
    , "3cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c18"
    , "20195108182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e801864"
    , "1a000eaf1f121a002a6e06061a0006be98011a0321aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a00"
    , "1e743c18241a0031410f0c1a000dbf9e011a09f2f6d31910d318241a0004578218241a096e44021967b518241a0473cee818"
    , "241a13e62472011a0f23d40118481a00212c5618481a0022814619fc3b041a00032b00192076041a0013be0419702c183f00"
    , "011a000f59d919aa6718fb00011a000187551902d61902cf00011a000187551902d61902cf00011a000187551902d61902cf"
    , "00011a0001a5661902a800011a00017468011a00044a391949a000011a0002bfe2189f01011a00026b371922ee00011a0002"
    , "6e9219226d00011a0001a3e2190ce2011a00019e4919028f011a001df8bb195fc803ff82d81e82190241192710d81e821902"
    , "d11a00989680821a009896801b00000002540be400821a02faf0801b00000009502f900019138818960385d81e8218331864"
    , "d81e820d14d81e820d14d81e8218331864d81e82183318648ad81e8218331864d81e820d14d81e820d14d81e820d14d81e82"
    , "18331864d81e8218331864d81e8218331864d81e8218331864d81e820304d81e82183318640114181e187b1a042c1d801864"
    , "d81e820f01981f182c1a00025ef51a00010000194000001a001e84801a1dcd6500121896d81e82030ad81e82031903e8d81e"
    , "820105820a001a1443fd001910d6a3009f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a0003"
    , "12591920a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a7"
    , "18201a00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a00037c71187a00"
    , "01011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e011999101903e819ecb2011a00022a47"
    , "18201a000144ce1820193bc318201a0001291101193371041956540a197147184a01197147184a0119a9151902280119aecd"
    , "19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a18201a00014f581a00037c71187a"
    , "0001011a0001614219020700011a000122c118201a00014f581a00037c71187a0001011a00014f581a00037c71187a000101"
    , "1a0004213c19583c041a00163cad19fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e8"
    , "6a1820194eae182019600c1820195108182019654d182019602f18201a032e93af1937fd0aff019f1a000189b41901a40101"
    , "1903e818ad00011903e819ea350401192baf18201a000312591920a404193e801864193e801864193e801864193e80186419"
    , "3e801864193e80186418641864193e8018641a000170a718201a00020782182019f016041a0001194a18b200011956871820"
    , "1a0001643519030104021a00014f581a00037c71187a0001011903e819a7a90402195fe419733a1826011a000db464196a8f"
    , "0119ca3f19022e011999101903e819ecb2011a00022a4718201a000144ce1820193bc318201a000129110119337104195654"
    , "0a197147184a01197147184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b18"
    , "20191cdf1820192d1a18201a00014f581a00037c71187a0001011a0001614219020700011a000122c118201a00014f581a00"
    , "037c71187a0001011a00014f581a00037c71187a0001011a000e94721a0003414000021a0004213c19583c041a00163cad19"
    , "fc3604194ff30104001a00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c18201951"
    , "08182019654d182019602f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a1a0013be0419702c183f00011a"
    , "000f59d919aa6718fb0001ff029f1a000189b41901a401011903e818ad00011903e819ea350401192baf18201a0003125919"
    , "20a404193e801864193e801864193e801864193e801864193e801864193e80186418641864193e8018641a000170a718201a"
    , "00020782182019f016041a0001194a18b2000119568718201a0001643519030104021a00014f581a0001e143191c89390383"
    , "1906b419022518391a00014f580001011903e819a7a90402195fe419733a1826011a000db464196a8f0119ca3f19022e0119"
    , "99101903e819ecb2011a00022a4718201a000144ce1820193bc318201a0001291101193371041956540a197147184a011971"
    , "47184a0119a9151902280119aecd19021d0119843c18201a00010a9618201a00011aaa1820191c4b1820191cdf1820192d1a"
    , "18201a00014f581a0001e143191c893903831906b419022518391a00014f5800011a0001614219020700011a000122c11820"
    , "1a00014f581a0001e143191c893903831906b419022518391a00014f580001011a00014f581a0001e143191c893903831906"
    , "b419022518391a00014f5800011a000e94721a0003414000021a0004213c19583c041a00163cad19fc3604194ff30104001a"
    , "00022aa818201a000189b41901a401011a00013eff182019e86a1820194eae182019600c1820195108182019654d18201960"
    , "2f18201a0290f1e70a1a032e93af1937fd0a1a0298e40b1966c40a193e801864193e8018641a000eaf1f121a002a6e06061a"
    , "0006be98011a0321aac7190eac121a00041699121a048e466e1922a4121a0327ec9a121a001e743c18241a0031410f0c1a00"
    , "0dbf9e011a09f2f6d31910d318241a0004578218241a096e44021967b518241a0473cee818241a13e62472011a0f23d40118"
    , "481a00212c5618481a0022814619fc3b041a00032b00192076041a0013be0419702c183f00011a000f59d919aa6718fb0001"
    , "1a000187551902d61902cf00011a000187551902d61902cf00011a000187551902d61902cf00011a0001a5661902a800011a"
    , "00017468011a00044a391949a000011a0002bfe2189f01011a00026b371922ee00011a00026e9219226d00011a0001a3e219"
    , "0ce2011a00019e4919028f011a001df8bb195fc803ff82d81e82190241192710d81e821902d11a00989680821a009896801b"
    , "00000002540be400821a02faf0801b00000009502f900019138818960385d81e8218331864d81e820d14d81e820d14d81e82"
    , "18331864d81e82183318648ad81e8218331864d81e820d14d81e820d14d81e820d14d81e8218331864d81e8218331864d81e"
    , "8218331864d81e8218331864d81e820304d81e82183318640114181e187b1a042c1d801864d81e820f0100a0848080808080"
    , "d9010280f4a18200581cc489488f55eea3db7a3c3441b57cb8c57518e99323fdd00f9ec494c21b009fdf42b6a24d9a008482"
    , "a18200581c01010101010101010101010101010101010101010101010101010101821828581c777777777777777777777777"
    , "77777777777777777777777777777777a082a0a082a0a00082a0008082a001f6"
    ]
