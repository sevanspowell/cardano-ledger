{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.TxBody
  ( MATxBody
      ( MATxBody,
        TxBodyConstr,
        TxBody',
        adHash',
        certs',
        inputs',
        mint',
        outputs',
        txfee',
        update',
        vldt',
        wdrls'
      ),
    TxBody,
    ShelleyMAEraTxBody(..),
    TxBodyRaw (..),
    txSparse,
    bodyFields,
    StrictMaybe (..),
    fromSJust,
    ValidityInterval (..),
    validateTimelock,
    initial,
  )
where

import Data.Proxy
import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TxBody)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization (encodeFoldable)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Tx (ShelleyWitnesses, WitnessSetHKD (..))
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    ShelleyEraTxBody (..),
    ShelleyTxOut (..),
    Wdrl (..),
    addrEitherShelleyTxOutL,
    valueEitherShelleyTxOutL,
  )
import Cardano.Ledger.ShelleyMA.Era
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock, ValidityInterval (..), evalTimelock)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeMint (..),
    EncodeMint (..),
    Val (..),
  )
import Control.DeepSeq (NFData (..))
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    decodeSet,
    decodeStrictSeq,
    encodeKeyedStrictMaybe,
    field,
    invalidField,
    ofield,
    (!>),
  )
import qualified Data.Map.Strict as Map
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- =======================================================

data TxBodyRaw era = TxBodyRaw
  { inputs :: !(Set (TxIn (Crypto era))),
    outputs :: !(StrictSeq (ShelleyTxOut era)),
    certs :: !(StrictSeq (DCert (Crypto era))),
    wdrls :: !(Wdrl (Crypto era)),
    txfee :: !Coin,
    vldt :: !ValidityInterval, -- imported from Timelocks
    update :: !(StrictMaybe (Update era)),
    adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    mint :: !(Value era)
  }

deriving instance
  (NFData (Value era), EraTxOut era, NFData (PParamsUpdate era)) =>
  NFData (TxBodyRaw era)

deriving instance
  (EraTxBody era, Eq (PParamsUpdate era)) =>
  Eq (TxBodyRaw era)

deriving instance
  (EraTxBody era, Show (PParamsUpdate era)) =>
  Show (TxBodyRaw era)

deriving instance Generic (TxBodyRaw era)

deriving instance
  (NoThunks (Value era), NoThunks (PParamsUpdate era)) =>
  NoThunks (TxBodyRaw era)

instance
  (EraTxBody era, FromCBOR (PParamsUpdate era), DecodeMint (Value era)) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBodyRaw"
          initial
          bodyFields
          [(0, "inputs"), (1, "outputs"), (2, "txfee")]
      )

instance
  (EraTxBody era, FromCBOR (PParamsUpdate era), DecodeMint (Value era)) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

fromSJust :: StrictMaybe a -> a
fromSJust (SJust x) = x
fromSJust SNothing = error "SNothing in fromSJust"

-- Sparse encodings of TxBodyRaw, the key values are fixed by backwarad compatibility
-- concerns as we want the Shelley era TxBody to deserialise as a Shelley-ma TxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.

txSparse ::
  (EraTxBody era, ToCBOR (PParamsUpdate era), EncodeMint (Value era)) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
txSparse (TxBodyRaw inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
  Keyed (\i o f topx c w u h botx forg -> TxBodyRaw i o c w f (ValidityInterval botx topx) u h forg)
    !> Key 0 (E encodeFoldable inp) -- We don't have to send these in TxBodyX order
    !> Key 1 (E encodeFoldable out) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> encodeKeyedStrictMaybe 3 top
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 up
    !> encodeKeyedStrictMaybe 7 hash
    !> encodeKeyedStrictMaybe 8 bot
    !> Omit isZero (Key 9 (E encodeMint frge))

bodyFields ::
  (EraTxBody era, FromCBOR (PParamsUpdate era), DecodeMint (Value era)) =>
  Word ->
  Field (TxBodyRaw era)
bodyFields 0 = field (\x tx -> tx {inputs = x}) (D (decodeSet fromCBOR))
bodyFields 1 = field (\x tx -> tx {outputs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 2 = field (\x tx -> tx {txfee = x}) From
bodyFields 3 = ofield (\x tx -> tx {vldt = (vldt tx) {invalidHereafter = x}}) From
bodyFields 4 = field (\x tx -> tx {certs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 5 = field (\x tx -> tx {wdrls = x}) From
bodyFields 6 = ofield (\x tx -> tx {update = x}) From
bodyFields 7 = ofield (\x tx -> tx {adHash = x}) From
bodyFields 8 = ofield (\x tx -> tx {vldt = (vldt tx) {invalidBefore = x}}) From
bodyFields 9 = field (\x tx -> tx {mint = x}) (D decodeMint)
bodyFields n = invalidField n

initial :: (Val (Value era)) => TxBodyRaw era
initial =
  TxBodyRaw
    empty
    (fromList [])
    (fromList [])
    (Wdrl Map.empty)
    (Coin 0)
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
    zero

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern construtor.

newtype MATxBody e = TxBodyConstr (MemoBytes (TxBodyRaw e))
  deriving (Typeable)
  deriving newtype (SafeToHash)

type TxBody era = MATxBody era

{-# DEPRECATED TxBody "Use `MATxBody` instead" #-}

deriving instance Eq (MATxBody era)

deriving instance
  (EraTxBody era, Show (PParamsUpdate era)) =>
  Show (MATxBody era)

deriving instance Generic (MATxBody era)

deriving newtype instance
  (Typeable era, NoThunks (Value era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MATxBody era)

deriving newtype instance
  ( NFData (Value era),
    NFData (PParamsUpdate era),
    EraTxBody era
  ) =>
  NFData (MATxBody era)

deriving newtype instance (Typeable era) => ToCBOR (MATxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    (EraTxBody era, FromCBOR (PParamsUpdate era), DecodeMint (Value era)) =>
    FromCBOR (Annotator (MATxBody era))

instance (c ~ Crypto era, Era era) => HashAnnotated (MATxBody era) EraIndependentTxBody c

-- Make a Pattern so the newtype and the MemoBytes are hidden

pattern MATxBody ::
  (EraTxBody era, ToCBOR (PParamsUpdate era), EncodeMint (Value era)) =>
  Set (TxIn (Crypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Value era ->
  MATxBody era
pattern MATxBody inputs outputs certs wdrls txfee vldt update adHash mint <-
  TxBodyConstr
    ( Memo
        TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}
        _
      )
  where
    MATxBody inputs outputs certs wdrls txfee vldt update adHash mint =
      TxBodyConstr $
        memoBytes $
          txSparse
            TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}

{-# COMPLETE MATxBody #-}

-- | This pattern is for deconstruction only but accompanied with fields and
-- projection functions.
pattern TxBody' ::
  Set (TxIn (Crypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Value era ->
  MATxBody era
pattern TxBody' {inputs', outputs', certs', wdrls', txfee', vldt', update', adHash', mint'} <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { inputs = inputs',
            outputs = outputs',
            certs = certs',
            wdrls = wdrls',
            txfee = txfee',
            vldt = vldt',
            update = update',
            adHash = adHash',
            mint = mint'
          }
        _
      )

{-# COMPLETE TxBody' #-}

instance
  ( CC.Crypto crypto,
    MAClass ma crypto
  ) =>
  EraTxOut (ShelleyMAEra ma crypto)
  where
  type TxOut (ShelleyMAEra ma crypto) = ShelleyTxOut (ShelleyMAEra ma crypto)

  mkBasicTxOut = ShelleyTxOut
  addrEitherTxOutL = addrEitherShelleyTxOutL
  valueEitherTxOutL = valueEitherShelleyTxOutL

instance (CC.Crypto crypto, MAClass ma crypto) => EraWitnesses (ShelleyMAEra ma crypto) where
  type Witnesses (ShelleyMAEra ma crypto) = ShelleyWitnesses (ShelleyMAEra ma crypto)

  scriptWitsG = to scriptWits

  addrWitsG = to addrWits'

  bootAddrWitsG = to bootWits

instance
  ( MAClass ma crypto,
    FromCBOR (PParamsUpdate (ShelleyMAEra ma crypto)),
    DecodeMint (MAValue ma crypto),
    NFData (PParamsUpdate (ShelleyMAEra ma crypto))
  ) =>
  EraTxBody (ShelleyMAEra ma crypto)
  where
  type TxBody (ShelleyMAEra ma crypto) = MATxBody (ShelleyMAEra ma crypto)

  inputsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> inputs m)

  allInputsTxBodyG = inputsTxBodyG

  outputsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> outputs m)

  txFeeTxBodyG = to (\(TxBodyConstr (Memo m _)) -> txfee m)

  mintedTxBodyG = to (\(TxBodyConstr (Memo m _)) -> getScriptHash (Proxy @ma) (mint m))

  adHashTxBodyG = to (\(TxBodyConstr (Memo m _)) -> adHash m)

instance
  ( MAClass ma crypto,
    FromCBOR (PParamsUpdate (ShelleyMAEra ma crypto)),
    DecodeMint (MAValue ma crypto),
    NFData (PParamsUpdate (ShelleyMAEra ma crypto))
  ) =>
  ShelleyEraTxBody (ShelleyMAEra ma crypto)
  where
  wdrlsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> wdrls m)

  ttlTxBodyG = undefined -- FIXME: restrict at the type level.

  updateTxBodyG = to (\(TxBodyConstr (Memo m _)) -> update m)

  certsTxBodyG = to (\(TxBodyConstr (Memo m _)) -> certs m)

class ShelleyEraTxBody era => ShelleyMAEraTxBody era where
  vldtTxBodyG :: SimpleGetter (Core.TxBody era) ValidityInterval

  mintTxBodyG :: SimpleGetter (Core.TxBody era) (Value era)

instance
  ( MAClass ma crypto,
    FromCBOR (PParamsUpdate (ShelleyMAEra ma crypto)),
    DecodeMint (MAValue ma crypto),
    NFData (PParamsUpdate (ShelleyMAEra ma crypto))
  ) =>
  ShelleyMAEraTxBody (ShelleyMAEra ma crypto)
  where
  vldtTxBodyG = to (\(TxBodyConstr (Memo m _)) -> vldt m)

  mintTxBodyG = to (\(TxBodyConstr (Memo m _)) -> mint m)

-- ==================================================================
-- Promote the fields of TxBodyRaw to be fields of TxBody. Either
-- automatically or by hand. Both methods have drawbacks.

{-
instance HasField tag (TxBodyRaw e) c => HasField (tag::Symbol) (TxBody e) c where
   getField (TxBodyConstr (Memo x _)) = getField @tag x

-- The method above autmatically lifts the Hasfield instances from TxBodyRaw to TxBody
-- the problem is, if some other file imports this file, it needs to import both
-- the hidden type TxBodyRaw and its constructors like this
-- import Cardano.Ledger.ShelleyMA.TxBody(TxBodyRaw(..))     OR
-- import qualified Cardano.Ledger.ShelleyMA.TxBody as XXX
-- Both are very ugly, but at least in the second way, one doesn't need to know the name of TxBodyRaw
-- So instead we tediously write by hand explicit HasField instances for TxBody
-}

-- ========================================
-- WellFormed era (and a few other) instances
{-

instance Value era ~ value => HasField "mint" (TxBody era) value where
  getField (TxBodyConstr (Memo m _)) = getField @"mint" m
-}
-- =======================================================
-- Validating timelock scripts
-- We Assume that TxBody has field "vldt" that extracts a ValidityInterval
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, ShelleyMAEraTxBody era) =>
  Timelock (Crypto era) ->
  Core.Tx era ->
  Bool
validateTimelock timelock tx = evalFPS (tx ^. bodyTxG)
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxG . addrWitsG)
    evalFPS txBody = evalTimelock vhks (txBody ^. vldtTxBodyG) timelock
