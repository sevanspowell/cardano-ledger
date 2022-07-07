{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Era (ShelleyEra) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), Script, TranslationContext, Value)
import Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Shelley.Scripts (MultiSig)

data ShelleyEra crypto

instance CC.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance Value (ShelleyEra _c) = Coin

type instance Script (ShelleyEra c) = MultiSig c

type instance TranslationContext (ShelleyEra c) = ()
