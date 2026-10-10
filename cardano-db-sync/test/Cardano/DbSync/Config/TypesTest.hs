{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.DbSync.Config.TypesTest (tests) where

import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Gen as Gen
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Default.Class (Default (..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Config.Types"
      [ ("SyncInsertConfig FromJSON", prop_syncInsertConfigFromJSON)
      , ("SyncInsertConfig roundtrip", prop_syncInsertConfigRoundtrip)
      , ("isTxEnabled", prop_isTxEnabled)
      , ("hasLedger", prop_hasLedger)
      , ("shouldUseLedger", prop_shouldUseLedger)
      , ("isShelleyEnabled", prop_isShelleyEnabled)
      , ("isMultiAssetEnabled", prop_isMultiAssetEnabled)
      , ("isMetadataEnabled", prop_isMetadataEnabled)
      , ("isPlutusEnabled", prop_isPlutusEnabled)
      , ("skip_offchain_metadata_hash_check defaults to disable", prop_skipOffchainMetadataHashCheckDefault)
      , ("skip_offchain_metadata_hash_check parses enable/disable", prop_skipOffchainMetadataHashCheckParse)
      , ("skip_offchain_metadata_hash_check rejects other values", prop_skipOffchainMetadataHashCheckRejectsInvalid)
      , ("skip_offchain_metadata_hash_check overrides presets", prop_skipOffchainMetadataHashCheckPresetOverride)
      ]

prop_syncInsertConfigFromJSON :: Property
prop_syncInsertConfigFromJSON = property $ do
  json <- forAll genDefaultJson
  Aeson.fromJSON json === Aeson.Success (def :: SyncInsertConfig)

prop_syncInsertConfigRoundtrip :: Property
prop_syncInsertConfigRoundtrip = property $ do
  cfg <- forAll Gen.syncInsertConfig

  let isPreset preset = case cfg of
        SyncInsertConfig (Just p) _ -> p == preset
        _other -> False

  let isCustomConfig = case cfg of
        SyncInsertConfig Nothing _ -> True
        _other -> False

  cover 5 "full" (isPreset FullInsertPreset)
  cover 5 "only utxo" (isPreset OnlyUTxOInsertPreset)
  cover 5 "only gov" (isPreset OnlyGovInsertPreset)
  cover 5 "disable all" (isPreset DisableAllInsertPreset)
  cover 5 "custom config" isCustomConfig

  tripping cfg Aeson.encode Aeson.decode

prop_isTxEnabled :: Property
prop_isTxEnabled = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let txOutCfg = sioTxOut cfg

  -- TxOut is enabled if it is not TxOutDisable
  isTxOutEnabled txOutCfg === (txOutCfg /= TxOutDisable)

prop_hasLedger :: Property
prop_hasLedger = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let ledgerCfg = sioLedger cfg

  -- Ledger is enabled if it is not LedgerDisable
  hasLedger ledgerCfg === (ledgerCfg /= LedgerDisable)

prop_shouldUseLedger :: Property
prop_shouldUseLedger = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let ledgerCfg = sioLedger cfg

  -- Ledger is enabled if it is not LedgerDisable
  shouldUseLedger ledgerCfg === (ledgerCfg == LedgerEnable)

prop_isShelleyEnabled :: Property
prop_isShelleyEnabled = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let shelleyCfg = sioShelley cfg

  -- Shelley is enabled if it is not ShelleyDisable
  isShelleyEnabled shelleyCfg === (shelleyCfg /= ShelleyDisable)

prop_isMultiAssetEnabled :: Property
prop_isMultiAssetEnabled = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let multiAssetCfg = sioMultiAsset cfg

  -- MultiAsset is enabled if it is not MultiAssetDisable
  isMultiAssetEnabled multiAssetCfg === (multiAssetCfg /= MultiAssetDisable)

prop_isMetadataEnabled :: Property
prop_isMetadataEnabled = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let metadataCfg = sioMetadata cfg

  -- Metadata is enabled if it is not MetadataDisable
  isMetadataEnabled metadataCfg === (metadataCfg /= MetadataDisable)

prop_isPlutusEnabled :: Property
prop_isPlutusEnabled = property $ do
  cfg <- forAll Gen.syncInsertOptions
  let plutusCfg = sioPlutus cfg

  -- Plutus is enabled if it is not PlutusDisable
  isPlutusEnabled plutusCfg === (plutusCfg /= PlutusDisable)

prop_skipOffchainMetadataHashCheckDefault :: Property
prop_skipOffchainMetadataHashCheckDefault = withTests 1 $ property $ do
  -- The hash check must stay on unless explicitly disabled.
  for_
    [ def
    , fullInsertOptions
    , onlyUTxOInsertOptions
    , onlyGovInsertOptions
    , disableAllInsertOptions
    ]
    $ \opts ->
      sioSkipOffchainMetadataHashCheck opts === SkipOffchainMetadataHashCheckConfig False

  -- Absent key keeps the check on.
  fmap (sioSkipOffchainMetadataHashCheck . sicOptions) (Aeson.fromJSON [aesonQQ|{}|])
    === Aeson.Success (SkipOffchainMetadataHashCheckConfig False)

prop_skipOffchainMetadataHashCheckParse :: Property
prop_skipOffchainMetadataHashCheckParse = withTests 1 $ property $ do
  fmap
    (sioSkipOffchainMetadataHashCheck . sicOptions)
    (Aeson.fromJSON [aesonQQ|{ "skip_offchain_metadata_hash_check": "enable" }|])
    === Aeson.Success (SkipOffchainMetadataHashCheckConfig True)
  fmap
    (sioSkipOffchainMetadataHashCheck . sicOptions)
    (Aeson.fromJSON [aesonQQ|{ "skip_offchain_metadata_hash_check": "disable" }|])
    === Aeson.Success (SkipOffchainMetadataHashCheckConfig False)
  Aeson.toJSON (SkipOffchainMetadataHashCheckConfig True) === Aeson.String "enable"
  Aeson.toJSON (SkipOffchainMetadataHashCheckConfig False) === Aeson.String "disable"

prop_skipOffchainMetadataHashCheckRejectsInvalid :: Property
prop_skipOffchainMetadataHashCheckRejectsInvalid = withTests 1 $ property $ do
  for_
    [ [aesonQQ|{ "skip_offchain_metadata_hash_check": "yes" }|]
    , [aesonQQ|{ "skip_offchain_metadata_hash_check": true }|]
    , [aesonQQ|{ "skip_offchain_metadata_hash_check": "Enable" }|]
    ]
    $ \json -> do
      annotateShow json
      case Aeson.fromJSON json :: Aeson.Result SyncInsertConfig of
        Aeson.Error _ -> success
        Aeson.Success _ -> failure

prop_skipOffchainMetadataHashCheckPresetOverride :: Property
prop_skipOffchainMetadataHashCheckPresetOverride = withTests 1 $ property $ do
  let json =
        [aesonQQ|
          {
            "preset": "full",
            "offchain_pool_data": "enable",
            "offchain_vote_data": "enable",
            "skip_offchain_metadata_hash_check": "enable"
          }
        |]
  Aeson.fromJSON json
    === Aeson.Success
      ( SyncInsertConfig
          (Just FullInsertPreset)
          fullInsertOptions
            { sioOffchainPoolData = OffchainPoolDataConfig True
            , sioOffchainVoteData = OffchainVoteDataConfig True
            , sioSkipOffchainMetadataHashCheck = SkipOffchainMetadataHashCheckConfig True
            }
      )

-- | Various JSON values that should generate the default config
genDefaultJson :: Gen Aeson.Value
genDefaultJson =
  Gen.element
    [ [aesonQQ|
        {
          "tx_out": {
            "value": "enable",
            "use_address_table": false,
            "force_tx_in": false
          },
          "ledger": "enable",
          "shelley": {
            "enable": true,
            "stake_addresses": null
          },
          "multi_asset": {
            "enable": true,
            "policies": null
          },
          "metadata": {
            "enable": true,
            "keys": null
          },
          "plutus": {
            "enable": true,
            "script_hashes": null
          },
          "governance": "enable",
          "offchain_pool_data": "disable",
          "json_type": "text"
        }
      |]
    , [aesonQQ|
        { }
      |]
    , [aesonQQ|
        {
          "tx_out": {
            "value": "enable",
            "use_address_table": false
          },
          "ledger": "enable",
          "shelley": {
            "enable": true
          },
          "multi_asset": {
            "enable": true
          },
          "metadata": {
            "enable": true
          },
          "plutus": {
            "enable": true
          },
          "governance": "enable",
          "offchain_pool_data": "disable",
          "json_type": "text"
        }
      |]
    ]
