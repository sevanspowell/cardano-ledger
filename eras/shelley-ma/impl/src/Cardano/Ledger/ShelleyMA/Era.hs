{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.ShelleyMA.Era (ShelleyMAEra, MAClass (..), MaryOrAllegra (..)) where

import Cardano.Ledger.Compactible (CompactForm, Compactible)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), Script, Value)
import Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Mary.Value (MaryValue, policies, policyID)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.Val
import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Kind (Type)
import Data.Set as Set (Set, empty, map)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData (..))

-- | The Shelley Mary/Allegra eras
--   The uninhabited type that indexes both the Mary and Allegra Eras.
data ShelleyMAEra (ma :: MaryOrAllegra) c

-- Both eras are implemented within the same codebase, matching the formal
-- specification. They differ only in the @value@ type. Due to some annoying
-- issues with 'Coin' and 'Value' being of different kinds, we don't parametrise
-- over the value but instead over a closed kind 'MaryOrAllegra'. But this
-- should be transparent to the user.
data MaryOrAllegra = Mary | Allegra

-- | The MAClass provides a method and a type, which implement the differences
--   between the Mary and Allegra instances
class
  ( Typeable ma,
    CC.Crypto crypto,
    DecodeNonNegative (MAValue ma crypto),
    Compactible (MAValue ma crypto),
    Eq (CompactForm (MAValue ma crypto)),
    NFData (MAValue ma crypto),
    Show (MAValue ma crypto),
    Val (MAValue ma crypto),
    Eq (MAValue ma crypto),
    FromCBOR (MAValue ma crypto),
    ToCBOR (MAValue ma crypto)
  ) =>
  MAClass (ma :: MaryOrAllegra) crypto
  where
  type MAValue (ma :: MaryOrAllegra) crypto :: Type
  getScriptHash :: proxy ma -> MAValue ma crypto -> Set.Set (ScriptHash crypto)

-- type family MAValue (ma :: MaryOrAllegra) c :: Type where
--   MAValue 'Mary c = MaryValue c
--   MAValue 'Allegra c = Coin

instance CC.Crypto c => MAClass 'Mary c where
  type MAValue 'Mary c = MaryValue c
  getScriptHash _ x = Set.map policyID (policies x)

instance CC.Crypto c => MAClass 'Allegra c where
  type MAValue 'Allegra c = Coin
  getScriptHash _ _ = Set.empty

-- | The actual Mary and Allegra instances, rolled into one, the MAClass superclass
--   provides the era-specific code for where they differ.
instance MAClass ma c => Era (ShelleyMAEra ma c) where
  type Crypto (ShelleyMAEra ma c) = c

type instance Value (ShelleyMAEra ma c) = MAValue ma c

type instance Script (ShelleyMAEra (_ma :: MaryOrAllegra) c) = Timelock c
