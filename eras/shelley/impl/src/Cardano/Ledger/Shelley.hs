{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Definition of the shelley era, along with instances ot the @Core@ types
-- defined in @module Cardano.Ledger.Core@, and instances of the @API@ classes
-- exposed in @module Cardano.Ledger.Shelley.API@.
module Cardano.Ledger.Shelley
  ( ShelleyEra,
    ShelleyTx,
    ShelleyTxOut,
    ShelleyTxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    ShelleyPParams,
    Witnesses,
    nativeMultiSigTag,
    -- Deprecated
    Shelley.Tx,
    Shelley.TxOut,
    Shelley.TxBody,
  )
where

import Cardano.Ledger.Core hiding (PParams)
import qualified Cardano.Ledger.Core as E (TranslationContext)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Shelley.BlockChain
  ( ShelleyTxSeq (..),
    bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (PParams, ShelleyPParams)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    ShelleyTxBody,
    ShelleyTxOut,
    validateNativeMultiSigScript,
  )
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx, TxBody, TxOut)
import qualified Data.ByteString as BS

type instance E.TranslationContext (ShelleyEra c) = ()

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

instance
  (CryptoClass.Crypto c) =>
  ValidateScript (ShelleyEra c)
  where
  scriptPrefixTag _script = nativeMultiSigTag

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  validateScript = validateNativeMultiSigScript

instance CryptoClass.Crypto c => SupportsSegWit (ShelleyEra c) where
  type TxSeq (ShelleyEra c) = ShelleyTxSeq (ShelleyEra c)
  fromTxSeq = txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = bbHash
  numSegComponents = 3
