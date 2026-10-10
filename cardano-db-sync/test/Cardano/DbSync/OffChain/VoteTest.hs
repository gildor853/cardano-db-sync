{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.VoteTest (tests) where

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Db as DB
import Cardano.DbSync.Error (runOrThrowIO)
import Cardano.DbSync.OffChain.Http (expectedMetaHashToCheck, parseAndValidateVoteData)
import qualified Cardano.DbSync.OffChain.Vote.Types as Vote
import Cardano.DbSync.Types (OffChainFetchError (..))
import Cardano.Prelude hiding ((%))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.OffChain.Vote"
      [ ("parseAndValidateVoteData handles valid CIP-119 format", prop_parseValidCIPFormat)
      , ("parseAndValidateVoteData handles invalid CIP format (type error)", prop_parseInvalidCIPFormat)
      , ("parseAndValidateVoteData handles valid JSON but invalid structure", prop_parseValidJsonInvalidStructure)
      , ("parseAndValidateVoteData handles unparseable JSON", prop_parseUnparseableJson)
      , ("image contentUrl base64 data URI keeps its prefix (#1966)", prop_imageDataUriKeepsPrefix)
      , ("parseAndValidateVoteData accepts a matching hash", prop_voteDataMatchingHash)
      , ("parseAndValidateVoteData rejects a hash mismatch when the check is on", prop_voteDataHashMismatchRejected)
      , ("parseAndValidateVoteData accepts a hash mismatch when the check is skipped", prop_voteDataHashMismatchSkipped)
      ]

-- | A hash that cannot match any of the test files.
wrongVoteMetaHash :: DB.VoteMetaHash
wrongVoteMetaHash = DB.VoteMetaHash (BS.replicate 32 0)

blake2b256 :: ByteString -> ByteString
blake2b256 = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256)

prop_voteDataMatchingHash :: Property
prop_voteDataMatchingHash = withTests 1 $ property $ do
  fileContent <- liftIO $ BS.readFile "test/testfiles/valid-vote-minimal.jsonld"
  let actualHash = blake2b256 fileContent
  for_ [False, True] $ \skipHashCheck -> do
    annotateShow skipHashCheck
    result <-
      liftIO $
        runExceptT $
          parseAndValidateVoteData fileContent (LBS.fromStrict fileContent) (expectedMetaHashToCheck skipHashCheck (DB.VoteMetaHash actualHash)) DB.DrepAnchor Nothing
    case result of
      Left err -> do
        annotateShow err
        failure
      Right (mocvd, _val, metaHash, _warning, isValidJson) -> do
        metaHash === actualHash
        assert isValidJson
        assert $ isJust mocvd

prop_voteDataHashMismatchRejected :: Property
prop_voteDataHashMismatchRejected = withTests 1 $ property $ do
  fileContent <- liftIO $ BS.readFile "test/testfiles/valid-vote-minimal.jsonld"
  result <-
    liftIO $
      runExceptT $
        parseAndValidateVoteData fileContent (LBS.fromStrict fileContent) (expectedMetaHashToCheck False wrongVoteMetaHash) DB.DrepAnchor Nothing
  case result of
    Left OCFErrHashMismatch {} -> success
    Left other -> do
      annotateShow other
      failure
    Right _ -> do
      annotate "Expected a hash mismatch error"
      failure

prop_voteDataHashMismatchSkipped :: Property
prop_voteDataHashMismatchSkipped = withTests 1 $ property $ do
  fileContent <- liftIO $ BS.readFile "test/testfiles/valid-vote-minimal.jsonld"
  result <-
    liftIO $
      runExceptT $
        parseAndValidateVoteData fileContent (LBS.fromStrict fileContent) (expectedMetaHashToCheck True wrongVoteMetaHash) DB.DrepAnchor Nothing
  case result of
    Left err -> do
      annotateShow err
      failure
    Right (mocvd, _val, metaHash, _warning, isValidJson) -> do
      -- The stored hash is the hash of the fetched bytes, not the on-chain hash.
      metaHash === blake2b256 fileContent
      assert isValidJson
      assert $ isJust mocvd

-- | Regression test for #1966: an inline base64 data URI contentUrl is stored verbatim, not stripped.
prop_imageDataUriKeepsPrefix :: Property
prop_imageDataUriKeepsPrefix = withTests 1 $ property $ do
  let dataUri =
        "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==" ::
          Text.Text
      payload = Aeson.encode (Aeson.object ["contentUrl" Aeson..= dataUri])
  annotate "image_url must keep the data:<mime>;base64, prefix"
  case Aeson.eitherDecode' payload :: Either [Char] Vote.Image of
    Left err -> annotate err >> failure
    Right img -> Vote.textValue (Vote.content img) === dataUri

-- | Test that we can parse valid CIP-119 format correctly
-- Scenario: Valid JSON + Valid CIP schema -> is_valid = true
prop_parseValidCIPFormat :: Property
prop_parseValidCIPFormat = withTests 1 $ property $ do
  -- Read the test file with valid CIP-119 format
  fileContent <- liftIO $ BS.readFile "test/testfiles/valid-vote-minimal.jsonld"
  let lbsContent = LBS.fromStrict fileContent

  -- Run the parser
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData fileContent lbsContent Nothing DB.DrepAnchor Nothing

  let (mocvd, val, _hash, _warning, isValidJson) = result

  -- Should succeed in parsing generic JSON
  annotate "Successfully parsed as generic JSON"
  assert isValidJson

  -- Should successfully parse into strongly-typed OffChainVoteData
  case mocvd of
    Just _ocvd -> do
      annotate "Successfully parsed into OffChainVoteData"
      success
    Nothing -> do
      annotate "Failed to parse into OffChainVoteData"
      failure

  -- Should have valid Aeson.Value
  case Aeson.toJSON val of
    Aeson.Object _obj -> do
      annotate "Has valid JSON object"
      success
    _ -> do
      annotate "Expected JSON object"
      failure

-- | Test that we can parse JSON with incorrect field types (e.g., doNotList as string instead of bool)
-- This is based on the issue https://github.com/IntersectMBO/cardano-db-sync/issues/1995
-- Scenario: Valid JSON but invalid CIP schema -> is_valid = false
prop_parseInvalidCIPFormat :: Property
prop_parseInvalidCIPFormat = withTests 1 $ property $ do
  -- Read the test file with invalid doNotList field (string instead of bool)
  fileContent <- liftIO $ BS.readFile "test/testfiles/invalid-vote-type-error.jsonld"
  let lbsContent = LBS.fromStrict fileContent

  -- Run the parser
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData fileContent lbsContent Nothing DB.DrepAnchor Nothing

  let (mocvd, val, _hash, _warning, isValidJson) = result

  -- Should succeed in parsing generic JSON
  annotate "Successfully parsed as generic JSON"
  assert isValidJson

  -- Should fail to parse into strongly-typed OffChainVoteData
  assert $ isNothing mocvd

  -- But should have valid Aeson.Value
  case Aeson.toJSON val of
    Aeson.Object _obj -> do
      annotate "Has valid JSON object"
      success
    _ -> do
      annotate "Expected JSON object"
      failure

-- | Test with completely valid JSON but not matching the CIP schema
-- Scenario: Valid JSON but invalid CIP schema -> is_valid = false
prop_parseValidJsonInvalidStructure :: Property
prop_parseValidJsonInvalidStructure = withTests 1 $ property $ do
  -- Read the test file with valid JSON but wrong structure
  fileContent <- liftIO $ BS.readFile "test/testfiles/invalid-vote-wrong-structure.jsonld"
  let lbsContent = LBS.fromStrict fileContent

  -- This should succeed because it's valid JSON, just not matching the schema
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData fileContent lbsContent Nothing DB.DrepAnchor Nothing

  let (mocvd, _val, _hash, _warning, isValidJson) = result

  annotate "Successfully parsed generic JSON"
  assert isValidJson
  -- Should not parse into OffChainVoteData
  assert $ isNothing mocvd

-- | Test with completely unparseable content (not valid JSON at all)
-- Scenario: Invalid JSON but hash matches -> is_valid = NULL
prop_parseUnparseableJson :: Property
prop_parseUnparseableJson = withTests 1 $ property $ do
  -- Read the test file with malformed JSON
  fileContent <- liftIO $ BS.readFile "test/testfiles/invalid-vote-malformed-json.jsonld"
  let lbsContent = LBS.fromStrict fileContent

  -- This should not fail, but instead return an error message in the JSON field
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData fileContent lbsContent Nothing DB.DrepAnchor Nothing

  let (mocvd, val, _hash, _warning, isValidJson) = result

  annotate "Content is not valid JSON"
  -- Should flag as invalid JSON
  assert $ not isValidJson

  -- Should not parse into OffChainVoteData
  assert $ isNothing mocvd

  -- Should have an error message in the JSON value
  case val of
    Aeson.Object obj -> do
      annotate "Has error message object"
      -- Check that error field exists
      case KeyMap.lookup (AesonKey.fromString "error") obj of
        Just (Aeson.String msg) -> do
          annotate $ "Error message: " <> show msg
          assert $ Text.isInfixOf "not valid JSON" msg
        _ -> do
          annotate "Expected error field with string value"
          failure
    _ -> do
      annotate "Expected JSON object with error message"
      failure
