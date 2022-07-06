{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Tx
  ( -- transaction
    Tx,
    ShelleyTx
      ( ShelleyTx,
        body,
        wits,
        auxiliaryData
      ),
    TxBody,
    ShelleyTxBody (..),
    TxOut,
    ShelleyTxOut (..),
    TxIn (..),
    TxId (..),
    decodeWits,
    segwitTx,
    -- witness data
    WitnessSet,
    WitnessSetHKD
      ( WitnessSet,
        addrWits,
        bootWits,
        scriptWits,
        txWitsBytes
      ),
    WitVKey (..),
    ValidateScript (..), -- reexported from Cardano.Ledger.Era
    txwitsScript,
    extractKeyHashWitnessSet,
    addrWits',
    evalNativeMultiSigScript,
    hashMultiSigScript,
    validateNativeMultiSigScript,
    prettyWitnessSetParts,
    minfee,
    witsFromTxWitnesses,
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeNull,
    encodePreEncoded,
    encodeWord,
    serialize,
    serializeEncoding,
    withSlice,
  )
import Cardano.Ledger.BaseTypes
  ( maybeToStrictMaybe,
  )
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core hiding (Tx, TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness, bootstrapWitKeyHash)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..), witVKeyHash)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Shelley.Metadata ()
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..), ShelleyTxOut (..), TxBody, TxOut)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Coders hiding (to)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict (StrictMaybe, strictMaybeToMaybe)
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import Lens.Micro (to, (^.))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- ========================================================

data TxRaw era = TxRaw
  { _body :: !(Core.TxBody era),
    _wits :: !(Witnesses era),
    _auxiliaryData :: !(StrictMaybe (AuxiliaryData era))
  }
  deriving (Generic, Typeable)

instance
  ( NFData (Core.TxBody era),
    NFData (Witnesses era),
    NFData (AuxiliaryData era)
  ) =>
  NFData (TxRaw era)

deriving instance
  ( Era era,
    Eq (AuxiliaryData era),
    Eq (Core.TxBody era),
    Eq (Witnesses era)
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Show (AuxiliaryData era),
    Show (Core.TxBody era),
    Show (Witnesses era)
  ) =>
  Show (TxRaw era)

instance
  ( Era era,
    NoThunks (AuxiliaryData era),
    NoThunks (Core.TxBody era),
    NoThunks (Witnesses era)
  ) =>
  NoThunks (TxRaw era)

newtype ShelleyTx era = TxConstr (MemoBytes (TxRaw era))
  deriving newtype (SafeToHash, ToCBOR)

type Tx era = ShelleyTx era

{-# DEPRECATED Tx "Use `ShelleyTx` instead" #-}

instance CC.Crypto crypto => EraTx (ShelleyEra crypto) where
  type Tx (ShelleyEra crypto) = ShelleyTx (ShelleyEra crypto)

  bodyTxG = to (\(TxConstr (Memo tx _)) -> _body tx)

  witsTxG = to (\(TxConstr (Memo tx _)) -> _wits tx)

  auxiliaryDataTxG = to (\(TxConstr (Memo tx _)) -> _auxiliaryData tx)

  sizeTxG = to (\(TxConstr (Memo _ bytes)) -> fromIntegral $ SBS.length bytes)

deriving newtype instance
  ( NFData (Core.TxBody (ShelleyEra crypto)),
    NFData (Witnesses (ShelleyEra crypto)),
    NFData (AuxiliaryData (ShelleyEra crypto)),
    CC.Crypto crypto
  ) =>
  NFData (ShelleyTx (ShelleyEra crypto))

deriving newtype instance Eq (Tx (ShelleyEra crypto))

deriving newtype instance
  ( CC.Crypto crypto,
    Show (AuxiliaryData (ShelleyEra crypto)),
    Show (Core.TxBody (ShelleyEra crypto)),
    Show (Witnesses (ShelleyEra crypto))
  ) =>
  Show (ShelleyTx (ShelleyEra crypto))

deriving newtype instance
  ( CC.Crypto crypto,
    NoThunks (AuxiliaryData (ShelleyEra crypto)),
    NoThunks (Core.TxBody (ShelleyEra crypto)),
    NoThunks (Witnesses (ShelleyEra crypto))
  ) =>
  NoThunks (ShelleyTx (ShelleyEra crypto))

pattern ShelleyTx ::
  ( CC.Crypto crypto,
    ToCBOR (AuxiliaryData (ShelleyEra crypto)),
    ToCBOR (Core.TxBody (ShelleyEra crypto)),
    ToCBOR (Witnesses (ShelleyEra crypto))
  ) =>
  Core.TxBody (ShelleyEra crypto) ->
  Witnesses (ShelleyEra crypto) ->
  StrictMaybe (AuxiliaryData (ShelleyEra crypto)) ->
  ShelleyTx (ShelleyEra crypto)
pattern ShelleyTx {body, wits, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { _body = body,
            _wits = wits,
            _auxiliaryData = auxiliaryData
          }
        _
      )
  where
    ShelleyTx b w a = TxConstr $ memoBytes (encodeTxRaw $ TxRaw b w a)

{-# COMPLETE ShelleyTx #-}

--------------------------------------------------------------------------------
-- Field accessors
--------------------------------------------------------------------------------
{-
instance
  aux ~ Core.AuxiliaryData era =>
  HasField "auxiliaryData" (Tx era) (StrictMaybe aux)
  where
  getField (TxConstr (Memo (TxRaw _ _ a) _)) = a

instance (body ~ Core.TxBody era) => HasField "body" (Tx era) body where
  getField (TxConstr (Memo (TxRaw b _ _) _)) = b

instance
  (wits ~ Core.Witnesses era) =>
  HasField "wits" (Tx era) wits
  where
  getField (TxConstr (Memo (TxRaw _ w _) _)) = w

instance HasField "txsize" (Tx era) Integer where
  getField (TxConstr (Memo _ bytes)) = fromIntegral $ SBS.length bytes
-}
--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxRaw ::
  ( ToCBOR (AuxiliaryData era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Witnesses era)
  ) =>
  TxRaw era ->
  Encode ('Closed 'Dense) (TxRaw era)
encodeTxRaw TxRaw {_body, _wits, _auxiliaryData} =
  Rec TxRaw
    !> To _body
    !> To _wits
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) _auxiliaryData

instance
  ( Era era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (AuxiliaryData era)),
    FromCBOR (Annotator (Witnesses era))
  ) =>
  FromCBOR (Annotator (TxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via
  Mem (TxRaw (ShelleyEra crypto))
  instance
    ( CC.Crypto crypto,
      FromCBOR (Annotator (Core.TxBody (ShelleyEra crypto))),
      FromCBOR (Annotator (AuxiliaryData (ShelleyEra crypto))),
      FromCBOR (Annotator (Witnesses (ShelleyEra crypto)))
    ) =>
    FromCBOR (Annotator (Tx (ShelleyEra crypto)))

-- | Construct a Tx containing the explicit serialised bytes.
--
--   This function is marked as unsafe since it makes no guarantee that the
--   represented bytes are indeed the correct serialisation of the transaction.
--   Thus, when calling this function, the caller is responsible for making this
--   guarantee.
--
--   The only intended use case for this is for segregated witness.
unsafeConstructTxWithBytes ::
  Core.TxBody era ->
  Witnesses era ->
  StrictMaybe (AuxiliaryData era) ->
  SBS.ShortByteString ->
  Tx era
unsafeConstructTxWithBytes b w a bytes = TxConstr (Memo (TxRaw b w a) bytes)

--------------------------------------------------------------------------------
-- Witnessing
--------------------------------------------------------------------------------

data WitnessSetHKD f era = WitnessSet'
  { addrWits' :: !(HKD f (Set (WitVKey 'Witness (Crypto era)))),
    scriptWits' :: !(HKD f (Map (ScriptHash (Crypto era)) (Script era))),
    bootWits' :: !(HKD f (Set (BootstrapWitness (Crypto era)))),
    txWitsBytes :: BSL.ByteString
  }

deriving instance
  (Era era, Show (Script era)) =>
  Show (WitnessSetHKD Identity era)

deriving instance
  (Era era, Eq (Script era)) =>
  Eq (WitnessSetHKD Identity era)

deriving instance Era era => Generic (WitnessSetHKD Identity era)

instance
  ( Era era,
    NFData (Script era),
    NFData (WitVKey 'Witness (Crypto era)),
    NFData (BootstrapWitness (Crypto era))
  ) =>
  NFData (WitnessSetHKD Identity era)

deriving via
  AllowThunksIn
    '[ "txWitsBytes"
     ]
    (WitnessSetHKD Identity era)
  instance
    (Era era, NoThunks (Script era)) =>
    (NoThunks (WitnessSetHKD Identity era))

type WitnessSet = WitnessSetHKD Identity

{-# DEPRECATED WitnessSet "Use `ShelleyWitnesses` instead" #-}

type ShelleyWitnesses = WitnessSetHKD Identity

instance CC.Crypto crypto => EraWitnesses (ShelleyEra crypto) where
  type Witnesses (ShelleyEra crypto) = ShelleyWitnesses (ShelleyEra crypto)

  scriptWitsG = to scriptWits'

  addrWitsG = to addrWits'

  bootAddrWitsG = to bootWits'

instance Era era => ToCBOR (WitnessSetHKD Identity era) where
  toCBOR = encodePreEncoded . BSL.toStrict . txWitsBytes

instance
  (Era era, ToCBOR (Script era)) =>
  Semigroup (WitnessSetHKD Identity era)
  where
  (WitnessSet' a b c _) <> y | Set.null a && Map.null b && Set.null c = y
  y <> (WitnessSet' a b c _) | Set.null a && Map.null b && Set.null c = y
  (WitnessSet a b c) <> (WitnessSet a' b' c') =
    WitnessSet (a <> a') (b <> b') (c <> c')

instance
  (Era era, ToCBOR (Script era)) =>
  Monoid (WitnessSetHKD Identity era)
  where
  mempty = WitnessSet mempty mempty mempty

pattern WitnessSet ::
  (Era era, ToCBOR (Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Script era) ->
  Set (BootstrapWitness (Crypto era)) ->
  WitnessSet era
pattern WitnessSet {addrWits, scriptWits, bootWits} <-
  WitnessSet' addrWits scriptWits bootWits _
  where
    WitnessSet awits scriptWitMap bootstrapWits =
      let encodeMapElement ix enc x =
            if null x then Nothing else Just (encodeWord ix <> enc x)
          l =
            catMaybes
              [ encodeMapElement 0 encodeFoldable awits,
                encodeMapElement 1 encodeFoldable scriptWitMap,
                encodeMapElement 2 encodeFoldable bootstrapWits
              ]
          n = fromIntegral $ length l
          witsBytes = serializeEncoding $ encodeMapLen n <> fold l
       in WitnessSet'
            { addrWits' = awits,
              scriptWits' = scriptWitMap,
              bootWits' = bootstrapWits,
              txWitsBytes = witsBytes
            }

{-# COMPLETE WitnessSet #-}

instance SafeToHash (WitnessSetHKD Identity era) where
  originalBytes = BSL.toStrict . txWitsBytes

-- | Exports the relevant parts from a (WintessSetHKD Identity era) for
--     use by the pretty printer without all the horrible constraints.
--     Uses the non-exported WitnessSet' constructor.
prettyWitnessSetParts ::
  WitnessSetHKD Identity era ->
  ( Set (WitVKey 'Witness (Crypto era)),
    Map (ScriptHash (Crypto era)) (Core.Script era),
    Set (BootstrapWitness (Crypto era))
  )
prettyWitnessSetParts (WitnessSet' a b c _) = (a, b, c)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

segwitTx ::
  ( ToCBOR (Core.TxBody era),
    ToCBOR (Witnesses era),
    ToCBOR (AuxiliaryData era)
  ) =>
  Annotator (Core.TxBody era) ->
  Annotator (Witnesses era) ->
  Maybe (Annotator (AuxiliaryData era)) ->
  Annotator (Tx era)
segwitTx
  bodyAnn
  witsAnn
  metaAnn = Annotator $ \bytes ->
    let body' = runAnnotator bodyAnn bytes
        witnessSet = runAnnotator witsAnn bytes
        metadata = flip runAnnotator bytes <$> metaAnn
        wrappedMetadataBytes = case metadata of
          Nothing -> serializeEncoding encodeNull
          Just b -> serialize b
        fullBytes =
          serializeEncoding (encodeListLen 3)
            <> serialize body'
            <> serialize witnessSet
            <> wrappedMetadataBytes
     in unsafeConstructTxWithBytes
          body'
          witnessSet
          (maybeToStrictMaybe metadata)
          (SBS.toShort . BSL.toStrict $ fullBytes)

instance
  ( Typeable era,
    FromCBOR (Annotator (Script era)),
    ValidateScript era
  ) =>
  FromCBOR (Annotator (WitnessSetHKD Identity era))
  where
  fromCBOR = decodeWits

-- | This type is only used to preserve the old buggy behavior where signature
-- was ignored in the `Ord` instance for `WitVKey`s.
newtype IgnoreSigOrd kr crypto = IgnoreSigOrd {unIgnoreSigOrd :: WitVKey kr crypto}
  deriving (Eq)

instance (Typeable kr, CC.Crypto crypto) => Ord (IgnoreSigOrd kr crypto) where
  compare (IgnoreSigOrd w1) (IgnoreSigOrd w2) = compare (witVKeyHash w1) (witVKeyHash w2)

decodeWits ::
  forall era s.
  ( FromCBOR (Annotator (Script era)),
    ValidateScript era
  ) =>
  Decoder s (Annotator (WitnessSet era))
decodeWits = do
  (mapParts, annBytes) <-
    withSlice $
      decodeMapContents $
        decodeWord >>= \case
          0 ->
            decodeList fromCBOR >>= \x ->
              pure
                ( \ws ->
                    ws
                      { addrWits' =
                          Set.map unIgnoreSigOrd . Set.fromList . fmap IgnoreSigOrd <$> sequence x
                      }
                )
          1 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {scriptWits' = keyBy (hashScript @era) <$> sequence x})
          2 ->
            decodeList fromCBOR >>= \x ->
              pure (\ws -> ws {bootWits' = Set.fromList <$> sequence x})
          k -> invalidKey k
  let witSet = foldr ($) emptyWitnessSetHKD mapParts
      emptyWitnessSetHKD :: WitnessSetHKD Annotator era
      emptyWitnessSetHKD =
        WitnessSet'
          { addrWits' = pure mempty,
            scriptWits' = pure mempty,
            bootWits' = pure mempty,
            txWitsBytes = mempty
          }
  pure $
    WitnessSet'
      <$> addrWits' witSet
      <*> scriptWits' witSet
      <*> bootWits' witSet
      <*> annBytes

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

-- ===============================================================

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  forall era.
  ( ValidateScript era,
    Script era ~ MultiSig (Crypto era)
  ) =>
  MultiSig (Crypto era) ->
  ScriptHash (Crypto era)
hashMultiSigScript = hashScript @era

-- ========================================

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript ::
  CC.Crypto crypto =>
  MultiSig crypto ->
  Set (KeyHash 'Witness crypto) ->
  Bool
evalNativeMultiSigScript (RequireSignature hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (RequireAllOf msigs) vhks =
  all (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireAnyOf msigs) vhks =
  any (`evalNativeMultiSigScript` vhks) msigs
evalNativeMultiSigScript (RequireMOf m msigs) vhks =
  m <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

-- | Script validator for native multi-signature scheme.
validateNativeMultiSigScript ::
  forall crypto.
  CC.Crypto crypto =>
  MultiSig crypto ->
  Tx (ShelleyEra crypto) ->
  Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig (coerceKeyRole `Set.map` vhks)
  where
    vhks = Set.map witVKeyHash (addrWits' (tx ^. witsTxG))

-- | Multi-signature script witness accessor function for Transactions
txwitsScript ::
  CC.Crypto crypto =>
  Tx (ShelleyEra crypto) ->
  Map (ScriptHash crypto) (Script (ShelleyEra crypto))
txwitsScript tx = tx ^. witsTxG . scriptWitsG

extractKeyHashWitnessSet ::
  forall (r :: KeyRole) crypto.
  [Credential r crypto] ->
  Set (KeyHash 'Witness crypto)
extractKeyHashWitnessSet = foldr accum Set.empty
  where
    accum (KeyHashObj hk) ans = Set.insert (asWitness hk) ans
    accum _other ans = ans

-- | Minimum fee calculation
minfee ::
  ( EraTx era,
    HasField "_minfeeA" pp Natural,
    HasField "_minfeeB" pp Natural
  ) =>
  pp ->
  Core.Tx era ->
  Coin
minfee pp tx =
  Coin $
    fromIntegral (getField @"_minfeeA" pp)
      * tx ^. sizeTxG + fromIntegral (getField @"_minfeeB" pp)

-- | Extract the witness hashes from the Transaction.
witsFromTxWitnesses ::
  EraTx era =>
  Core.Tx era ->
  Set (KeyHash 'Witness (Crypto era))
witsFromTxWitnesses tx =
  Set.map witVKeyHash (tx ^. witsTxG . addrWitsG)
    `Set.union` Set.map bootstrapWitKeyHash (tx ^. witsTxG . bootAddrWitsG)
