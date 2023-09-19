{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Tracker.Models.LBSPDatum where

import qualified PlutusTx
import qualified Prelude as Haskell
import qualified GHC.Generics as GHC
import Plutus.V2.Ledger.Api (PubKeyHash, Datum (Datum), FromData (fromBuiltinData))
import ErgoDex.Class (FromLedger(parseFromLedger))
import CardanoTx.Models (FullTxOut(..), TxOutDatum (..))
import Data.Maybe (Maybe(..))
import ErgoDex.State (OnChain(OnChain))
import Data.Function (($))
import Data.Aeson (ToJSON, FromJSON)

data LBSPDatum = LBSPDatum
  { users :: [[PubKeyHash]]
  } deriving stock (Haskell.Show, GHC.Generic, Haskell.Eq)
    deriving (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''LBSPDatum [('LBSPDatum, 0)]
PlutusTx.makeLift ''LBSPDatum

instance FromLedger LBSPDatum where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(KnownDatum (Datum d)), ..} =
    case fromBuiltinData d of
        (Just lbsp@LBSPDatum{..}) -> Just $ OnChain fout lbsp
        _ -> Nothing
  parseFromLedger _ = Nothing