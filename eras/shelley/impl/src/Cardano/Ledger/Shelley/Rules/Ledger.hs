{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Ledger
  ( LEDGER,
    LedgerEnv (..),
    LedgerPredicateFailure (..),
    LedgerEvent (..),
    Event,
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes (ShelleyBase, TxIx, invalidKey)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DPState (..),
    DState (..),
    LedgerState (..),
    PState (..),
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.Rules.Delegs
  ( DELEGS,
    DelegsEnv (..),
    DelegsEvent,
    DelegsPredicateFailure,
  )
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UTXOW, UtxowPredicateFailure)
import Cardano.Ledger.Shelley.TxBody (DCert, ShelleyEraTxBody (..))
import Cardano.Ledger.Slot (SlotNo)
import Control.DeepSeq (NFData (..))
import Control.State.Transition
  ( Assertion (..),
    AssertionViolation (..),
    Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Coders (decodeRecordSum)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- ========================================================

data LEDGER era

data LedgerEnv era = LedgerEnv
  { ledgerSlotNo :: !SlotNo,
    ledgerIx :: !TxIx,
    ledgerPp :: !(PParams era),
    ledgerAccount :: !AccountState
  }

deriving instance Show (PParams era) => Show (LedgerEnv era)

instance NFData (PParams era) => NFData (LedgerEnv era) where
  rnf (LedgerEnv _slotNo _ix pp _account) = rnf pp

data LedgerPredicateFailure era
  = UtxowFailure (PredicateFailure (EraRule "UTXOW" era)) -- Subtransition Failures
  | DelegsFailure (PredicateFailure (EraRule "DELEGS" era)) -- Subtransition Failures
  deriving (Generic)

data LedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | DelegsEvent (Event (EraRule "DELEGS" era))

deriving stock instance
  ( Show (PredicateFailure (EraRule "DELEGS" era)),
    Show (PredicateFailure (EraRule "UTXOW" era)),
    Era era
  ) =>
  Show (LedgerPredicateFailure era)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "DELEGS" era)),
    Eq (PredicateFailure (EraRule "UTXOW" era)),
    Era era
  ) =>
  Eq (LedgerPredicateFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "DELEGS" era)),
    NoThunks (PredicateFailure (EraRule "UTXOW" era)),
    Era era
  ) =>
  NoThunks (LedgerPredicateFailure era)

instance
  ( ToCBOR (PredicateFailure (EraRule "DELEGS" era)),
    ToCBOR (PredicateFailure (EraRule "UTXOW" era)),
    Era era
  ) =>
  ToCBOR (LedgerPredicateFailure era)
  where
  toCBOR = \case
    (UtxowFailure a) -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a
    (DelegsFailure a) -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR a

instance
  ( FromCBOR (PredicateFailure (EraRule "DELEGS" era)),
    FromCBOR (PredicateFailure (EraRule "UTXOW" era)),
    Era era
  ) =>
  FromCBOR (LedgerPredicateFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailure (LEDGER era)" $
      \case
        0 -> do
          a <- fromCBOR
          pure (2, UtxowFailure a)
        1 -> do
          a <- fromCBOR
          pure (2, DelegsFailure a)
        k -> invalidKey k

instance
  ( Show (PParams era),
    Show (Tx era),
    Show (TxOut era),
    Show (State (EraRule "PPUP" era)),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    EraTx era,
    ShelleyEraTxBody era,
    Embed (EraRule "DELEGS" era) (LEDGER era),
    Embed (EraRule "UTXOW" era) (LEDGER era),
    Environment (EraRule "UTXOW" era) ~ UtxoEnv era,
    State (EraRule "UTXOW" era) ~ UTxOState era,
    Signal (EraRule "UTXOW" era) ~ Tx era,
    Environment (EraRule "DELEGS" era) ~ DelegsEnv era,
    State (EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin
  ) =>
  STS (LEDGER era)
  where
  type State (LEDGER era) = LedgerState era
  type Signal (LEDGER era) = Tx era
  type Environment (LEDGER era) = LedgerEnv era
  type BaseM (LEDGER era) = ShelleyBase
  type PredicateFailure (LEDGER era) = LedgerPredicateFailure era
  type Event (LEDGER era) = LedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation (" <> avSTS <> "): " <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (LedgerEnv {ledgerPp}, _, _))
           (LedgerState utxoSt DPState {dpsDState, dpsPState}) ->
              obligation ledgerPp (rewards dpsDState) (_pParams dpsPState) -- FIX ME
                == _deposited utxoSt
        )
    ]

ledgerTransition ::
  forall era.
  ( EraTx era,
    ShelleyEraTxBody era,
    Embed (EraRule "DELEGS" era) (LEDGER era),
    Environment (EraRule "DELEGS" era) ~ DelegsEnv era,
    State (EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (EraRule "UTXOW" era) (LEDGER era),
    Environment (EraRule "UTXOW" era) ~ UtxoEnv era,
    State (EraRule "UTXOW" era) ~ UTxOState era,
    Signal (EraRule "UTXOW" era) ~ Tx era
  ) =>
  TransitionRule (LEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt dpstate, tx) <- judgmentContext

  dpstate' <-
    trans @(EraRule "DELEGS" era) $
      TRC
        ( DelegsEnv slot txIx pp tx account,
          dpstate,
          StrictSeq.fromStrict $ tx ^. bodyTxG . certsTxBodyG
        )

  let DPState dstate pstate = dpstate
      genDelegs = _genDelegs dstate
      stpools = _pParams pstate

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv slot pp stpools genDelegs,
          utxoSt,
          tx
        )
  pure (LedgerState utxoSt' dpstate')

instance
  ( Era era,
    STS (DELEGS era),
    PredicateFailure (EraRule "DELEGS" era) ~ DelegsPredicateFailure era,
    Event (EraRule "DELEGS" era) ~ DelegsEvent era
  ) =>
  Embed (DELEGS era) (LEDGER era)
  where
  wrapFailed = DelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (EraRule "UTXOW" era) ~ UtxowPredicateFailure era,
    Event (EraRule "UTXOW" era) ~ Event (UTXOW era)
  ) =>
  Embed (UTXOW era) (LEDGER era)
  where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

-- =============================================================
