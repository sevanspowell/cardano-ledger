{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Core (ShelleyEra) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), Script, Value)
import Cardano.Ledger.Crypto as CryptoClass (Crypto)
import Cardano.Ledger.Shelley.Scripts (MultiSig)

data ShelleyEra crypto

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance Value (ShelleyEra _c) = Coin

type instance Script (ShelleyEra c) = MultiSig c
