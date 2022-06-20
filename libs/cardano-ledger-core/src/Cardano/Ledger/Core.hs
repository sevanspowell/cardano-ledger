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
    PParams,
    PParamsDelta,

    -- * Constraint synonyms
    ChainData,
    SerialisableData,
    AnnotatedData,

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
import Cardano.Ledger.AuxiliaryData ( AuxiliaryDataHash )
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
import NoThunks.Class (NoThunks)

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

  txBodyG :: SimpleGetter (Tx era) (TxBody era)

  txWitsG :: SimpleGetter (Tx era) (Witnesses era)

  txAuxiliaryDataG :: SimpleGetter (Tx era) (StrictMaybe (AuxiliaryData era))

  txSizeG :: SimpleGetter (Tx era) Integer

class
  ( EraTxOut era,
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era),
    NFData (TxBody era),
    Eq (TxBody era)
  ) =>
  EraTxBody era
  where
  -- | The body of a transaction.
  type TxBody era = (r :: Type) | r -> era

  txBodyInputsG :: SimpleGetter (TxBody era) (Set (TxIn (Crypto era)))

  txBodyOutputsG :: SimpleGetter (TxBody era) (StrictSeq (TxOut era))

  txBodyTxFeeG :: SimpleGetter (TxBody era) Coin

  txBodyMintedG :: SimpleGetter (TxBody era) (Set (ScriptHash (Crypto era)))

  txBodyAllInputsG :: SimpleGetter (TxBody era) (Set (TxIn (Crypto era)))

  txBodyAdHashG :: SimpleGetter (TxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era)))

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

  txOutCoinL :: Lens' (TxOut era) Coin
  txOutCoinL =
    lens
      (\txOut -> coin (txOut ^. txOutValueL))
      (\txOut c -> txOut & txOutValueL .~ modifyCoin (const c) (txOut ^. txOutValueL))

  txOutValueL :: Lens' (TxOut era) (Value era)
  txOutValueL =
    lens
      ( \txOut -> case txOut ^. txOutValueEitherL of
          Left value -> value
          Right cValue -> fromCompact cValue
      )
      (\txOut value -> txOut & txOutValueEitherL .~ Left value)

  txOutCompactValueL :: Lens' (TxOut era) (CompactForm (Value era))
  txOutCompactValueL =
    lens
      ( \txOut -> case txOut ^. txOutValueEitherL of
          Left value ->
            fromMaybe (error $ "Illegal value in TxOut: " <> show value) $ toCompact value
          Right cValue -> cValue
      )
      (\txOut cValue -> txOut & txOutValueEitherL .~ Right cValue)

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  txOutValueEitherL :: Lens' (TxOut era) (Either (Value era) (CompactForm (Value era)))

  txOutAddrL :: Lens' (TxOut era) (Addr (Crypto era))
  txOutAddrL =
    lens
      ( \txOut -> case txOut ^. txOutAddrEitherL of
          Left addr -> addr
          Right cAddr -> decompactAddr cAddr
      )
      (\txOut addr -> txOut & txOutAddrEitherL .~ Left addr)

  txOutCompactAddrL :: Lens' (TxOut era) (CompactAddr (Crypto era))
  txOutCompactAddrL =
    lens
      ( \txOut -> case txOut ^. txOutAddrEitherL of
          Left addr -> compactAddr addr
          Right cAddr -> cAddr
      )
      (\txOut cAddr -> txOut & txOutAddrEitherL .~ Right cAddr)

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  --
  -- The utility of this function comes from the fact that TxOut usually stores
  -- the address in either one of two forms: compacted or unpacked. In order to
  -- avoid extroneous conversions in `getTxOutAddr` and `getTxOutCompactAddr` we
  -- can define just this functionality. Also sometimes it crutial to know at
  -- the callsite which form of address we have readily available without any
  -- conversions (eg. searching millions of TxOuts for a particular address)
  txOutAddrEitherL :: Lens' (TxOut era) (Either (Addr (Crypto era)) (CompactAddr (Crypto era)))

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

-- | Protocol parameters
type family PParams era = (r :: Type) | r -> era

-- | The type of updates to Protocol parameters
type family PParamsDelta era = (r :: Type) | r -> era

-- | The set of witnesses in a Tx
class Era era => EraWitnesses era where
  type Witnesses era = (r :: Type) | r -> era
  addrWitsG :: SimpleGetter (Witnesses era) (Set (WitVKey 'Witness (Crypto era)))

  bootAddrWitsG :: SimpleGetter (Witnesses era) (Set (BootstrapWitness (Crypto era)))

  scriptWitsG :: SimpleGetter (Witnesses era) (Map (ScriptHash (Crypto era)) (Script era))

-- | Common constraints
--
-- NOTE: 'Ord' is not included, as 'Ord' for a 'Block' or a 'NewEpochState'
-- doesn't make sense.
type ChainData t = (Eq t, Show t, NoThunks t, Typeable t)

-- | Constraints for serialising from/to CBOR
type SerialisableData t = (FromCBOR t, ToCBOR t)

-- | Constraints for serialising from/to CBOR using 'Annotator'
type AnnotatedData t = (FromCBOR (Annotator t), ToCBOR t)

-- | Era STS map
type family EraRule (k :: Symbol) era :: Type

--------------------------------------------------------

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

class (CC.Crypto (Crypto era), Typeable era) => Era era where
  type Crypto era :: Type

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
--   This class is embodies an isomorphism between 'TxSeq era' and 'StrictSeq
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

-- ==========================================================
-- WellFormed-ness
-- ==========================================================

{-
-- | All Well Formed Eras have this minimal structure.
type WellFormed era =
  ( -- TxBody
    HasField "outputs" (TxBody era) (StrictSeq (TxOut era)),
    HasField "txfee" (TxBody era) Coin,
    HasField "minted" (TxBody era) (Set (ScriptHash (Crypto era))),
    -- HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era))),
    -- Tx
    HasField "body" (Tx era) (TxBody era),
    HasField "wits" (Tx era) (Witnesses era),
    HasField "auxiliaryData" (Tx era) (StrictMaybe (AuxiliaryData era)),
    HasField "txsize" (Tx era) Integer,
    HasField "scriptWits" (Tx era) (Map (ScriptHash (Crypto era)) (Script era)),
    -- HashAnnotated
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era),
    SupportsSegWit era,
    Val (Value era),
    Compactible (Value era) -- TxOut stores a CompactForm(Value)
  )
-}
{-  TODO, there are a few other constraints which are WellFormed and we should add
them when time permits. Some are not added because the types they mentions reside
in files that cause circular import dependencies.
   -- import Cardano.Ledger.Shelley.TxBody(DCert,Wdrl,WitVKey)
   -- import Cardano.Ledger.Shelley.Tx(TxIn)
These would have to be moved into a module such as Cardano.Ledger.TxBase(TxIn,DCert,Wdrl)
   -- HasField "inputs" (TxBody era) (Set (TxIn (Crypto era))),       -- all possible inputs
   -- HasField "txinputs_fee" (TxBody era) (Set (TxIn (Crypto era)))  -- inputs that can be used to pay fees
   -- HasField "certs" (TxBody era) (StrictSeq (DCert (Crypto era))),
   -- HasField "wdrls" (TxBody era) (Wdrl (Crypto era)),
   -- HasField "addrWits" (Tx era) (Set (WitVKey 'Witness (Crypto era)))
others where the concrete type (Update and WitnessSet) will have to be made into a type family
   -- import Cardano.Ledger.Shelley.PParams (Update)
   -- import Cardano.Ledger.Shelley.Tx(WitnessSet)
   -- import Cardano.Ledger.Alonzo.Scripts (ExUnits)
   -- HasField "update" (TxBody era) (StrictMaybe (Update era)),
   -- HasField "wits" (Tx era) (WitnessSet era),
   -- HasField "exUnits" (Tx era) ExUnits,
-}
