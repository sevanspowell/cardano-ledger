{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
    PoolMetadata (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    StakePoolRelay (..),
    TxBody,
    ShelleyTxBody
      ( ShelleyTxBody,
        TxBodyConstr,
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash
      ),
    ShelleyEraTxBody (..),
    TxBodyRaw (..),
    EraIndependentTxBody,
    TxOut,
    ShelleyTxOut (ShelleyTxOut, TxOutCompact),
    Url,
    Wdrl (..),
    --
    module Cardano.Ledger.Keys.WitVKey,
    witKeyHash,
    wvkBytes,
    --
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Url)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Compactible (Compactible (CompactForm, fromCompact, toCompact))
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization
  ( decodeRecordNamed,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.PoolParams
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (DecodeNonNegative (..))
import Cardano.Prelude (HeapWords (..))
import Control.DeepSeq (NFData (rnf))
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (ShortByteString, pack)
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    encode,
    encodeKeyedStrictMaybe,
    field,
    invalidField,
    ofield,
    (!>),
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

-- ========================================================================

newtype Wdrl crypto = Wdrl {unWdrl :: Map (RewardAcnt crypto) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData)

instance CC.Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = mapToCBOR . unWdrl

instance CC.Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl <$> mapFromCBOR

data ShelleyTxOut era = TxOutCompact
  { txOutCompactAddr :: {-# UNPACK #-} !(CompactAddr (Crypto era)),
    txOutCompactValue :: !(CompactForm (Value era))
  }

type TxOut era = ShelleyTxOut era

{-# DEPRECATED TxOut "Use `ShelleyTxOut` instead" #-}

instance CC.Crypto crypto => EraTxOut (ShelleyEra crypto) where
  type TxOut (ShelleyEra crypto) = ShelleyTxOut (ShelleyEra crypto)

  mkBasicTxOut = ShelleyTxOut

  addrEitherTxOutL =
    lens
      (Right . txOutCompactAddr)
      ( \txOut -> \case
          Left addr -> txOut {txOutCompactAddr = compactAddr addr}
          Right cAddr -> txOut {txOutCompactAddr = cAddr}
      )
  valueEitherTxOutL =
    lens
      (Right . txOutCompactValue)
      ( \txOut -> \case
          Left value ->
            txOut
              { txOutCompactValue =
                  fromMaybe (error $ "Illegal value in TxOut: " <> show value) $ toCompact value
              }
          Right cValue -> txOut {txOutCompactValue = cValue}
      )

-- assume Shelley+ type address : payment addr, staking addr (same length as payment), plus 1 word overhead
instance (Era era, HeapWords (CompactForm (Value era))) => HeapWords (TxOut era) where
  heapWords (TxOutCompact _ vl) =
    3
      + heapWords (packedADDRHASH (Proxy :: Proxy era))
      + heapWords vl

-- a ShortByteString of the same length as the ADDRHASH
-- used to calculate heapWords
packedADDRHASH :: forall proxy era. (CC.Crypto (Crypto era)) => proxy era -> ShortByteString
packedADDRHASH _ = pack (replicate (fromIntegral (1 + 2 * HS.sizeHash (Proxy :: Proxy (CC.ADDRHASH (Crypto era))))) (1 :: Word8))

instance
  (EraTxOut era) =>
  Show (TxOut era)
  where
  show = show . viewCompactTxOut -- FIXME: showing TxOut as a tuple is just sad

deriving instance Eq (TxOut (ShelleyEra crypto))

instance NFData (TxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern ShelleyTxOut ::
  (HasCallStack, EraTxOut era) =>
  Addr (Crypto era) ->
  Value era ->
  TxOut era
pattern ShelleyTxOut addr vl <-
  (viewCompactTxOut -> (addr, vl))
  where
    ShelleyTxOut addr vl =
      TxOutCompact
        (compactAddr addr)
        (fromMaybe (error $ "Illegal value in TxOut: " <> show vl) $ toCompact vl)

{-# COMPLETE ShelleyTxOut #-}

viewCompactTxOut ::
  EraTxOut era =>
  TxOut era ->
  (Addr (Crypto era), Value era)
viewCompactTxOut TxOutCompact {txOutCompactAddr, txOutCompactValue} =
  (decompactAddr txOutCompactAddr, fromCompact txOutCompactValue)

-- ---------------------------
-- WellFormed instances

-- ==============================
-- The underlying type for TxBody

data TxBodyRaw era = TxBodyRaw
  { _inputsX :: !(Set (TxIn (Crypto era))),
    _outputsX :: !(StrictSeq (TxOut era)),
    _certsX :: !(StrictSeq (DCert (Crypto era))),
    _wdrlsX :: !(Wdrl (Crypto era)),
    _txfeeX :: !Coin,
    _ttlX :: !SlotNo,
    _txUpdateX :: !(StrictMaybe (Update era)),
    _mdHashX :: !(StrictMaybe (AuxiliaryDataHash (Crypto era)))
  }
  deriving (Generic, Typeable)

deriving instance
  NoThunks (PParamsUpdate (ShelleyEra crypto)) =>
  NoThunks (TxBodyRaw (ShelleyEra crypto))

deriving instance
  (CC.Crypto crypto, NFData (PParamsUpdate (ShelleyEra crypto))) =>
  NFData (TxBodyRaw (ShelleyEra crypto))

deriving instance
  (CC.Crypto crypto, Eq (PParamsUpdate (ShelleyEra crypto))) =>
  Eq (TxBodyRaw (ShelleyEra crypto))

deriving instance
  ( CC.Crypto crypto,
    Show (PParamsUpdate (ShelleyEra crypto))
  ) =>
  Show (TxBodyRaw (ShelleyEra crypto))

instance
  (FromCBOR (PParamsUpdate (ShelleyEra crypto)), CC.Crypto crypto) =>
  FromCBOR (TxBodyRaw (ShelleyEra crypto))
  where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          baseTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]
      )

instance
  ( ToCBOR (PParamsUpdate (ShelleyEra crypto)),
    Typeable crypto,
    CC.Crypto crypto,
    FromCBOR (PParamsUpdate (ShelleyEra crypto))
  ) =>
  FromCBOR (Annotator (TxBodyRaw (ShelleyEra crypto)))
  where
  fromCBOR = pure <$> fromCBOR

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody ::
  (FromCBOR (PParamsUpdate (ShelleyEra crypto)), CC.Crypto crypto) =>
  Word ->
  Field (TxBodyRaw (ShelleyEra crypto))
boxBody 0 = field (\x tx -> tx {_inputsX = x}) (D (decodeSet fromCBOR))
boxBody 1 = field (\x tx -> tx {_outputsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 4 = field (\x tx -> tx {_certsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 5 = field (\x tx -> tx {_wdrlsX = x}) From
boxBody 2 = field (\x tx -> tx {_txfeeX = x}) From
boxBody 3 = field (\x tx -> tx {_ttlX = x}) From
boxBody 6 = ofield (\x tx -> tx {_txUpdateX = x}) From
boxBody 7 = ofield (\x tx -> tx {_mdHashX = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (ToCBOR (PParamsUpdate (ShelleyEra crypto)), CC.Crypto crypto) =>
  TxBodyRaw (ShelleyEra crypto) ->
  Encode ('Closed 'Sparse) (TxBodyRaw (ShelleyEra crypto))
txSparse (TxBodyRaw input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> TxBodyRaw i o c w f t u h)
    !> Key 0 (E encodeFoldable input) -- We don't have to send these in TxBodyRaw order
    !> Key 1 (E encodeFoldable output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 update
    !> encodeKeyedStrictMaybe 7 hash

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
baseTxBodyRaw :: TxBodyRaw (ShelleyEra crypto)
baseTxBodyRaw =
  TxBodyRaw
    { _inputsX = Set.empty,
      _outputsX = StrictSeq.empty,
      _txfeeX = Coin 0,
      _ttlX = SlotNo 0,
      _certsX = StrictSeq.empty,
      _wdrlsX = Wdrl Map.empty,
      _txUpdateX = SNothing,
      _mdHashX = SNothing
    }

instance
  (ToCBOR (PParamsUpdate (ShelleyEra crypto)), CC.Crypto crypto) =>
  ToCBOR (TxBodyRaw (ShelleyEra crypto))
  where
  toCBOR x = encode (txSparse x)

-- ====================================================
-- Introduce ShelleyTxBody as a newtype around a MemoBytes

newtype ShelleyTxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (Generic, Typeable)
  deriving newtype (SafeToHash)

type TxBody era = ShelleyTxBody era

{-# DEPRECATED TxBody "Use `ShelleyTxBody` instead" #-}

instance CC.Crypto crypto => EraTxBody (ShelleyEra crypto) where
  type TxBody (ShelleyEra crypto) = ShelleyTxBody (ShelleyEra crypto)

  inputsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _inputsX m)

  allInputsTxBodyG = inputsTxBodyG

  outputsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _outputsX m)

  txFeeTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _txfeeX m)

  mintedTxBodyG = to (const Set.empty) -- TODO: fix this wart. Shelley does not know what minting is

  adHashTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _mdHashX m)

class EraTxBody era => ShelleyEraTxBody era where
  wdrlsTxBodyG :: SimpleGetter (Core.TxBody era) (Wdrl (Crypto era))

  ttlTxBodyG :: SimpleGetter (Core.TxBody era) SlotNo

  updateTxBodyG :: SimpleGetter (Core.TxBody era) (StrictMaybe (Update era))

  certsTxBodyG :: SimpleGetter (Core.TxBody era) (StrictSeq (DCert (Crypto era)))

instance CC.Crypto crypto => ShelleyEraTxBody (ShelleyEra crypto) where
  wdrlsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _wdrlsX m)

  ttlTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _ttlX m)

  updateTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _txUpdateX m)

  certsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> _certsX m)

deriving newtype instance
  ( Typeable crypto,
    NoThunks (PParamsUpdate (ShelleyEra crypto))
  ) =>
  NoThunks (TxBody (ShelleyEra crypto))

deriving newtype instance
  (CC.Crypto crypto, NFData (PParamsUpdate (ShelleyEra crypto))) =>
  NFData (TxBody (ShelleyEra crypto))

deriving instance
  ( CC.Crypto crypto,
    Show (PParamsUpdate (ShelleyEra crypto))
  ) =>
  Show (TxBody (ShelleyEra crypto))

deriving instance Eq (TxBody (ShelleyEra crypto))

deriving via
  (Mem (TxBodyRaw (ShelleyEra crypto)))
  instance
    ( Typeable crypto,
      CC.Crypto crypto,
      FromCBOR (PParamsUpdate (ShelleyEra crypto)),
      ToCBOR (PParamsUpdate (ShelleyEra crypto))
    ) =>
    FromCBOR (Annotator (TxBody (ShelleyEra crypto)))

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  (ToCBOR (PParamsUpdate (ShelleyEra crypto)), CC.Crypto crypto) =>
  Set (TxIn (Crypto (ShelleyEra crypto))) ->
  StrictSeq (TxOut (ShelleyEra crypto)) ->
  StrictSeq (DCert (Crypto (ShelleyEra crypto))) ->
  Wdrl (Crypto (ShelleyEra crypto)) ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update (ShelleyEra crypto)) ->
  StrictMaybe (AuxiliaryDataHash (Crypto (ShelleyEra crypto))) ->
  ShelleyTxBody (ShelleyEra crypto)
pattern ShelleyTxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { _inputsX = _inputs,
            _outputsX = _outputs,
            _certsX = _certs,
            _wdrlsX = _wdrls,
            _txfeeX = _txfee,
            _ttlX = _ttl,
            _txUpdateX = _txUpdate,
            _mdHashX = _mdHash
          }
        _
      )
  where
    ShelleyTxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      TxBodyConstr $ memoBytes (txSparse (TxBodyRaw _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash))

{-# COMPLETE ShelleyTxBody #-}

-- =========================================

instance HashAnnotated (TxBody (ShelleyEra crypto)) EraIndependentTxBody crypto

instance CC.Crypto crypto => ToCBOR (TxBody (ShelleyEra crypto)) where
  toCBOR (TxBodyConstr memo) = toCBOR memo

-- ===============================================================

instance CC.Crypto crypto => ToCBOR (TxOut (ShelleyEra crypto)) where
  toCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance CC.Crypto crypto => FromCBOR (TxOut (ShelleyEra crypto)) where
  fromCBOR = fromNotSharedCBOR

-- This instance does not do any sharing and is isomorphic to FromCBOR
-- use the weakest constraint necessary
instance (EraTxOut era, DecodeNonNegative (Value era)) => FromSharedCBOR (TxOut era) where
  type Share (TxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR _ =
    decodeRecordNamed "TxOut" (const 2) $ do
      cAddr <- fromCBOR
      coin <- decodeNonNegative
      pure $ TxOutCompact cAddr coin

witKeyHash :: WitVKey kr crypto -> KeyHash 'Witness crypto
witKeyHash = witVKeyHash
{-# DEPRECATED witKeyHash "In favor of `witVKeyHash`" #-}

wvkBytes :: WitVKey kr crypto -> BSL.ByteString
wvkBytes = witVKeyBytes
{-# DEPRECATED wvkBytes "In favor of `witVKeyBytes`" #-}
