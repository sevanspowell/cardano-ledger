{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Constraints where

import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
  ( AnnotatedData,
    ChainData,
    PParams,
    PParamsDelta,
    Script,
    SerialisableData,
    TxBody,
    Value,
  )
import Cardano.Ledger.Era (Crypto, Era (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (HashAnnotated)
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

type UsesTxBody era =
  ( Era era,
    ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era)
  )

class
  ( Era era,
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    SerialisableData (Value era),
    DecodeNonNegative (Value era),
    EncodeMint (Value era),
    DecodeMint (Value era)
  ) =>
  UsesValue era

-- class
--   ( Era era,
--     ChainData (TxOut era),
--     ToCBOR (TxOut era),
--     FromCBOR (TxOut era)
--   ) =>
--   UsesTxOut era
--   where
--   makeTxOut :: Proxy era -> Addr (Crypto era) -> Value era -> TxOut era

type UsesScript era =
  ( Era era,
    Eq (Script era),
    Show (Script era),
    AnnotatedData (Script era)
  )

-- type UsesAuxiliary era =
--   ( Era era,
--     Eq (AuxiliaryData era),
--     Show (AuxiliaryData era),
--     AnnotatedData (AuxiliaryData era)
--   )

class
  ( Era era,
    Eq (PParams era),
    Show (PParams era),
    SerialisableData (PParams era),
    ChainData (PParamsDelta era),
    Ord (PParamsDelta era),
    SerialisableData (PParamsDelta era)
  ) =>
  UsesPParams era
  where
  mergePPUpdates ::
    proxy era ->
    PParams era ->
    PParamsDelta era ->
    PParams era
