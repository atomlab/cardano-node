{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage () where

import           Cardano.Prelude
import qualified Prelude

import           Codec.Serialise (Serialise, serialise)
import           Data.Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Scientific (coefficient)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Cardano.Chain.Update as Update
import           Cardano.Slotting.Block (BlockNo (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Network.Block (HeaderHash, Tip (..))


deriving instance Show TracingVerbosity

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> panic $ "Parsing of TracingVerbosity failed, "
                 <> err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = panic $ "Parsing of TracingVerbosity failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON CoreNodeId where
  parseJSON v = CoreNodeId <$> parseJSON v


instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
                                 Just port -> pure port
                                 Nothing -> panic $ (show portNum)
                                                  <> " is not a valid port number."
  parseJSON invalid  = panic $ "Parsing of port number failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    panic $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance Serialise (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash (BlockNo bn)) =
    object
      [ "slotNo" .= slotNo
      , "headerHash" .=
          (Text.decodeLatin1 . Base16.encode . LBS.toStrict . serialise) headerHash
      , "blockNo" .= bn
      ]
