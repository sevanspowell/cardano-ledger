{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | API to the Shelley ledger
module Cardano.Ledger.Shelley.API
  ( module X,
    ShelleyBasedEra,
  )
where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation as X
import Cardano.Ledger.Shelley.API.Genesis as X
import Cardano.Ledger.Shelley.API.Mempool as X
import Cardano.Ledger.Shelley.API.Types as X
import Cardano.Ledger.Shelley.API.Validation as X
import Cardano.Ledger.Shelley.API.Wallet as X
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody)
import Data.Sharing (FromSharedCBOR, Interns, Share)

class
  ( ApplyBlock era,
    ApplyTx era,
    CanStartFromGenesis era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromSharedCBOR (Core.TxOut era),
    ShelleyEraTxBody era,
    Core.EraTx era
  ) =>
  ShelleyBasedEra era

instance
  ( CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  ) =>
  ShelleyBasedEra (ShelleyEra crypto)
