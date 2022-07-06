{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines core type families which we know to vary from era to
-- era.
--
-- Families in this module should be indexed on era.
--
-- It is intended for qualified import:
-- > import qualified Cardano.Ledger.Core as Core
module Cardano.Ledger.Core
  ( -- * Era-changing types
    EraTx (..),
    EraTxOut (..),
    EraTxBody (..),
    EraAuxiliaryData (..),
    EraWitnesses (..),
    Value,
    Script,
    EraPParams (..),

    -- * Era STS
    EraRule,
    Era (..),
    PreviousEra,
    TranslationContext,
    TranslateEra (..),
    translateEra',
    translateEraMaybe,
    ValidateScript (..),
    -- $segWit
    SupportsSegWit (..),
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.CompactAddress (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes
  ( EraIndependentAuxiliaryData,
    EraIndependentBlockBody,
    EraIndependentTxBody,
    ScriptHash (..),
  )
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.Keys.WitVKey (WitVKey)
import Cardano.Ledger.SafeHash
  ( HashAnnotated (..),
    SafeToHash (..),
  )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (DecodeNonNegative, Val (..))
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except, runExcept)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import Lens.Micro

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

class (CC.Crypto (Crypto era), Typeable era) => Era era where
  type Crypto era :: Type

-- | A transaction.
class
  ( EraTxBody era,
    EraWitnesses era,
    EraAuxiliaryData era,
    -- NFData (Tx era), TODO: Add NFData constraints to Crypto class
    Eq (Tx era)
  ) =>
  EraTx era
  where
  type Tx era = (r :: Type) | r -> era

  bodyTxG :: SimpleGetter (Tx era) (TxBody era)

  witsTxG :: SimpleGetter (Tx era) (Witnesses era)

  auxiliaryDataTxG :: SimpleGetter (Tx era) (StrictMaybe (AuxiliaryData era))

  sizeTxG :: SimpleGetter (Tx era) Integer

class
  ( EraTxOut era,
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era),
    FromCBOR (Annotator (TxBody era)),
    ToCBOR (TxBody era),
    NFData (TxBody era),
    Eq (TxBody era)
  ) =>
  EraTxBody era
  where
  -- | The body of a transaction.
  type TxBody era = (r :: Type) | r -> era

  inputsTxBodyG :: SimpleGetter (TxBody era) (Set (TxIn (Crypto era)))

  outputsTxBodyG :: SimpleGetter (TxBody era) (StrictSeq (TxOut era))

  txFeeTxBodyG :: SimpleGetter (TxBody era) Coin

  mintedTxBodyG :: SimpleGetter (TxBody era) (Set (ScriptHash (Crypto era)))

  allInputsTxBodyG :: SimpleGetter (TxBody era) (Set (TxIn (Crypto era)))

  adHashTxBodyG :: SimpleGetter (TxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era)))

-- | Abstract interface into specific fields of a `TxOut`
class
  ( DecodeNonNegative (Value era),
    Compactible (Value era),
    Show (Value era),
    Val (Value era),
    FromCBOR (Value era),
    ToCBOR (Value era),
    FromCBOR (TxOut era),
    ToCBOR (TxOut era),
    NFData (TxOut era),
    Eq (TxOut era),
    Era era
  ) =>
  EraTxOut era
  where
  -- | The output of a UTxO for a particular era
  type TxOut era = (r :: Type) | r -> era

  mkBasicTxOut :: Addr (Crypto era) -> Value era -> TxOut era

  coinTxOutL :: Lens' (TxOut era) Coin
  coinTxOutL =
    lens
      (\txOut -> coin (txOut ^. valueTxOutL))
      (\txOut c -> txOut & valueTxOutL .~ modifyCoin (const c) (txOut ^. valueTxOutL))

  valueTxOutL :: Lens' (TxOut era) (Value era)
  valueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> value
          Right cValue -> fromCompact cValue
      )
      (\txOut value -> txOut & valueEitherTxOutL .~ Left value)

  compactValueTxOutL :: Lens' (TxOut era) (CompactForm (Value era))
  compactValueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value ->
            fromMaybe (error $ "Illegal value in TxOut: " <> show value) $ toCompact value
          Right cValue -> cValue
      )
      (\txOut cValue -> txOut & valueEitherTxOutL .~ Right cValue)

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  valueEitherTxOutL :: Lens' (TxOut era) (Either (Value era) (CompactForm (Value era)))

  addrTxOutL :: Lens' (TxOut era) (Addr (Crypto era))
  addrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> addr
          Right cAddr -> decompactAddr cAddr
      )
      (\txOut addr -> txOut & addrEitherTxOutL .~ Left addr)

  compactAddrTxOutL :: Lens' (TxOut era) (CompactAddr (Crypto era))
  compactAddrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> compactAddr addr
          Right cAddr -> cAddr
      )
      (\txOut cAddr -> txOut & addrEitherTxOutL .~ Right cAddr)

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  --
  -- The utility of this function comes from the fact that TxOut usually stores
  -- the address in either one of two forms: compacted or unpacked. In order to
  -- avoid extroneous conversions in `getTxOutAddr` and `getTxOutCompactAddr` we
  -- can define just this functionality. Also sometimes it crutial to know at
  -- the callsite which form of address we have readily available without any
  -- conversions (eg. searching millions of TxOuts for a particular address)
  addrEitherTxOutL :: Lens' (TxOut era) (Either (Addr (Crypto era)) (CompactAddr (Crypto era)))

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | Scripts which may lock transaction outputs in this era
type family Script era :: Type

-- | AuxiliaryData which may be attached to a transaction
class
  ( Era era,
    HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData (Crypto era)
  ) =>
  EraAuxiliaryData era
  where
  type AuxiliaryData era = (r :: Type) | r -> era
  hashAuxiliaryData :: AuxiliaryData era -> AuxiliaryDataHash (Crypto era)
  validateAuxiliaryData :: ProtVer -> AuxiliaryData era -> Bool

class
  ( Era era,
    Eq (PParams era),
    Ord (PParamsUpdate era),
    Show (PParams era)
  ) =>
  EraPParams era
  where
  -- | Protocol parameters
  type PParams era = (r :: Type) | r -> era

  -- | The type of updates to Protocol parameters
  type PParamsUpdate era = (r :: Type) | r -> era

  applyPPUpdates :: PParams era -> PParamsUpdate era -> PParams era

-- | The set of witnesses in a Tx
class Era era => EraWitnesses era where
  type Witnesses era = (r :: Type) | r -> era
  addrWitsG :: SimpleGetter (Witnesses era) (Set (WitVKey 'Witness (Crypto era)))

  bootAddrWitsG :: SimpleGetter (Witnesses era) (Set (BootstrapWitness (Crypto era)))

  scriptWitsG :: SimpleGetter (Witnesses era) (Map (ScriptHash (Crypto era)) (Script era))

-- | Era STS map
type family EraRule (k :: Symbol) era :: Type

-----------------------------------------------------------------------------
-- Script Validation
-----------------------------------------------------------------------------

-- | Typeclass for script data types. Allows for script validation and hashing.
--   You must understand the role of SafeToHash and scriptPrefixTag to make new
--   instances. 'scriptPrefixTag' is a magic number representing the tag of the
--   script language. For each new script language defined, a new tag is chosen
--   and the tag is included in the script hash for a script. The safeToHash
--   constraint ensures that Scripts are never reserialised.
class (Era era, SafeToHash (Script era)) => ValidateScript era where
  scriptPrefixTag :: Script era -> BS.ByteString
  validateScript :: Script era -> Tx era -> Bool
  hashScript :: Script era -> ScriptHash (Crypto era)
  -- ONE SHOULD NOT OVERIDE THE hashScript DEFAULT METHOD
  -- UNLESS YOU UNDERSTAND THE SafeToHash class, AND THE ROLE OF THE scriptPrefixTag
  hashScript =
    ScriptHash . Hash.castHash
      . Hash.hashWith
        (\x -> scriptPrefixTag @era x <> originalBytes x)
  isNativeScript :: Script era -> Bool
  isNativeScript _ = True

--------------------------------------------------------------------------------
-- Segregated Witness
--------------------------------------------------------------------------------

-- $segWit
-- * Segregated Witness
--
-- The idea of segregated witnessing is to alter the encoding of transactions in
-- a block such that the witnesses (the information needed to verify the
-- validity of the transactions) can be stored separately from the body (the
-- information needed to update the ledger state). In this way, a node which
-- only cares about replaying transactions need not even decode the witness
-- information.
--
-- In order to do this, we introduce two concepts:
-- - A 'TxSeq`, which represents the decoded structure of a sequence of
--   transactions as represented in the encoded block; that is, with witnessing,
--   metadata and other non-body parts split separately.

-- | Indicates that an era supports segregated witnessing.
--
--   This class embodies an isomorphism between 'TxSeq era' and 'StrictSeq
--   (Tx era)', witnessed by 'fromTxSeq' and 'toTxSeq'.
class Era era => SupportsSegWit era where
  type TxSeq era = (r :: Type) | r -> era

  fromTxSeq :: TxSeq era -> StrictSeq (Tx era)
  toTxSeq :: StrictSeq (Tx era) -> TxSeq era

  -- | Get the block body hash from the TxSeq. Note that this is not a regular
  -- "hash the stored bytes" function since the block body hash forms a small
  -- Merkle tree.
  hashTxSeq ::
    TxSeq era ->
    Hash.Hash (CC.HASH (Crypto era)) EraIndependentBlockBody

  -- | The number of segregated components
  numSegComponents :: Word64

--------------------------------------------------------------------------------
-- Era translation
--------------------------------------------------------------------------------

-- | Map an era to its predecessor.
--
-- For example:
--
-- > type instance PreviousEra (AllegraEra c) = ShelleyEra c
type family PreviousEra era = (r :: Type) | r -> era

-- | Per-era context used for 'TranslateEra'.
--
-- This context will be passed to the translation instances of /all/ types of
-- that particular era. In practice, most instances won't need the context, but
-- this approach makes the translation composable (as opposed to having a
-- separate context per type).
type family TranslationContext era :: Type

-- | Translation of types between eras, e.g., from Shelley to Allegra.
--
-- When @era@ is just a phantom type parameter, an empty standalone deriving can be used:
--
-- > newtype Foo era = Foo Int
-- >
-- > instance TranslateEra (Allegra c) Foo
--
-- Note that one could use @DerivingAnyClass@ (@deriving (TranslateEra (Allegra
-- c))@), but this would introduce an undesired coupling between the
-- era-parametric type and (a) particular era(s). The intention is to have a
-- module with orphan instances per era.
--
-- In most cases, the @era@ parameter won't be phantom, and a manual instance
-- will have to be written:
--
-- > newtype Bar era = Bar (TxBody era)
-- >
-- > instance CC.Crypto c => TranslateEra (Allegra c) Bar where
-- >     translateEra ctxt = Bar <$> translateEra ctxt
-- >
-- > -- With the following instance being in scope:
-- > instance CC.Crypto c => TranslatEra (Allegra c) TxBody
--
-- Note: we use 'PreviousEra' instead of @NextEra@ as an era definitely knows
-- its predecessor, but not necessarily its successor. Moreover, one could argue
-- that it makes more sense to define the translation from era A to era B where
-- era B is defined, than where era A is defined.
class (Era era, Era (PreviousEra era)) => TranslateEra era f where
  -- | Most translations should be infallible (default instance), but we leave
  -- the door open for partial translations.
  --
  -- For a partial translation, override the default type to be '()' or a
  -- concrete error type.
  type TranslationError era f :: Type

  type TranslationError era f = Void

  -- | Translate a type @f@ parameterised by the era from an era to the era
  -- after it.
  --
  -- The translation is a given the translation context of @era@.
  --
  -- A default instance is provided for when the two types are 'Coercible'.
  translateEra :: TranslationContext era -> f (PreviousEra era) -> Except (TranslationError era f) (f era)
  default translateEra ::
    Coercible (f (PreviousEra era)) (f era) =>
    TranslationContext era ->
    f (PreviousEra era) ->
    Except (TranslationError era f) (f era)
  translateEra _ = return . coerce

-- | Variant of 'translateEra' for when 'TranslationError' is 'Void' and the
-- translation thus cannot fail.
translateEra' ::
  (TranslateEra era f, TranslationError era f ~ Void) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  f era
translateEra' ctxt = either absurd id . runExcept . translateEra ctxt

-- | Variant of 'translateEra' for when 'TranslationError' is '()', converting
-- the result to a 'Maybe'.
translateEraMaybe ::
  (TranslateEra era f, TranslationError era f ~ ()) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  Maybe (f era)
translateEraMaybe ctxt =
  either (const Nothing) Just . runExcept . translateEra ctxt
