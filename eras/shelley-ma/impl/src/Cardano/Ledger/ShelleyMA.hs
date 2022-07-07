{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA
  ( ShelleyMAEra,
    MaryOrAllegra (..),
    TxOut,
    TxBody,
    AuxiliaryData,
    Shelley.PParams,
    Tx,
  )
where

import Cardano.Ledger.AuxiliaryData
  ( AuxiliaryDataHash (..),
    ValidateAuxiliaryData (..),
  )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (..), SupportsSegWit (..), ValidateScript (..))
import Cardano.Ledger.Mary.Value (Value, policies, policyID)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( TxSeq (..),
    bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Metadata (validMetadatum)
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.Shelley.Tx (Tx, TxOut (..), WitnessSet)
import Cardano.Ledger.ShelleyMA.AuxiliaryData
  ( AuxiliaryData,
    pattern AuxiliaryData,
  )
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    validateTimelock,
  )
import Cardano.Ledger.ShelleyMA.TxBody (TxBody)
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (deepseq)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))

-- ========================================


instance CryptoClass.Crypto c => UsesValue (ShelleyMAEra 'Mary c)

instance CryptoClass.Crypto c => UsesValue (ShelleyMAEra 'Allegra c)

instance CryptoClass.Crypto c => UsesTxOut (ShelleyMAEra 'Mary c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesTxOut (ShelleyMAEra 'Allegra c) where
  makeTxOut _ a v = TxOut a v

instance CryptoClass.Crypto c => UsesPParams (ShelleyMAEra 'Mary c) where
  mergePPUpdates _ = Shelley.updatePParams

instance CryptoClass.Crypto c => UsesPParams (ShelleyMAEra 'Allegra c) where
  mergePPUpdates _ = Shelley.updatePParams

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------


type instance
  Core.Tx (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Tx (ShelleyMAEra ma c)

type instance
  Core.TxOut (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    TxOut (ShelleyMAEra ma c)

type instance
  Core.TxBody (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    TxBody (ShelleyMAEra ma c)


type instance
  Core.AuxiliaryData (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    AuxiliaryData (ShelleyMAEra (ma :: MaryOrAllegra) c)

type instance
  Core.PParams (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Shelley.PParams (ShelleyMAEra (ma :: MaryOrAllegra) c)

type instance
  Core.Witnesses (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    WitnessSet (ShelleyMAEra (ma :: MaryOrAllegra) c)

type instance
  Core.PParamsDelta (ShelleyMAEra (ma :: MaryOrAllegra) c) =
    Shelley.PParamsUpdate (ShelleyMAEra (ma :: MaryOrAllegra) c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- Since Timelock scripts are a strictly backwards compatible extension of
-- Multisig scripts, we can use the same 'scriptPrefixTag' tag here as
-- we did for the ValidateScript instance in Multisig which is imported
-- from:  Cardano.Ledger.Shelley(nativeMultiSigTag)

instance
  ( CryptoClass.Crypto c,
    UsesTxBody (ShelleyMAEra ma c),
    Core.AnnotatedData (Core.AuxiliaryData (ShelleyMAEra ma c))
  ) =>
  ValidateScript (ShelleyMAEra ma c)
  where
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  validateScript script tx = validateTimelock @(ShelleyMAEra ma c) script tx

-- Uses the default instance of hashScript

instance
  ( CryptoClass.Crypto c,
    MAClass ma c
  ) =>
  SupportsSegWit (ShelleyMAEra ma c)
  where
  type TxSeq (ShelleyMAEra ma c) = Shelley.TxSeq (ShelleyMAEra ma c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = Shelley.TxSeq
  hashTxSeq = Shelley.bbHash
  numSegComponents = 3

instance
  forall ma c.
  MAClass ma c =>
  HasField "minted" (TxBody (ShelleyMAEra (ma :: MaryOrAllegra) c)) (Set.Set (ScriptHash c))
  where
  getField x = getScriptHash (Proxy @ma) (getField @"mint" x)
