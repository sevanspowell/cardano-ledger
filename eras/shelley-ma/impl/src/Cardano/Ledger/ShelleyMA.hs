{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA
  ( ShelleyMAEra,
    MaryOrAllegra (..),
    ShelleyTx,
    ShelleyTxOut,
    MATxBody,
    MAAuxiliaryData,
    ShelleyPParams,

    -- * Deprecated
    Tx,
    TxOut,
    TxBody,
    PParams,
    AuxiliaryData,
  )
where

import Cardano.Ledger.Core hiding (AuxiliaryData, PParams, Tx, TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core (Tx)
import Cardano.Ledger.Shelley (nativeMultiSigTag)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.PParams (PParams, ShelleyPParams)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    ShelleyTxOut,
    Tx,
    TxOut,
    auxiliaryDataShelleyTxL,
    bodyShelleyTxL,
    sizeShelleyTxG,
    witsShelleyTxL,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AuxiliaryData, MAAuxiliaryData)
import Cardano.Ledger.ShelleyMA.Era (MAClass, MaryOrAllegra (..), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody, TxBody, validateTimelock)

-- ========================================

instance MAClass ma crypto => EraTx (ShelleyMAEra ma crypto) where
  type Tx (ShelleyMAEra ma crypto) = ShelleyTx (ShelleyMAEra ma crypto)

  bodyTxG = bodyShelleyTxL

  witsTxG = witsShelleyTxL

  auxiliaryDataTxG = auxiliaryDataShelleyTxL

  sizeTxG = sizeShelleyTxG

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- Since Timelock scripts are a strictly backwards compatible extension of
-- Multisig scripts, we can use the same 'scriptPrefixTag' tag here as
-- we did for the ValidateScript instance in Multisig

instance MAClass ma crypto => ValidateScript (ShelleyMAEra ma crypto) where
  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"
  validateScript script tx = validateTimelock @(ShelleyMAEra ma crypto) script tx

-- Uses the default instance of hashScript

instance MAClass ma crypto => SupportsSegWit (ShelleyMAEra ma crypto) where
  type TxSeq (ShelleyMAEra ma crypto) = ShelleyTxSeq (ShelleyMAEra ma crypto)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = Shelley.bbHash
  numSegComponents = 3
