{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module Vesting where

import Plutus.V2.Ledger.Api         (
                                    PubKeyHash, ScriptContext,
                                    POSIXTime, Validator, BuiltinData, TxInfo (txInfoValidRange),
                                    scriptContextTxInfo, mkValidatorScript
                                    )
import Plutus.V2.Ledger.Contexts    (txSignedBy)
import PlutusTx                     (unstableMakeIsData, compile)
import PlutusTx.Prelude             (traceIfFalse)
import Plutus.V1.Ledger.Interval    (contains, from)
import Prelude                      (Bool, ($), (&&), IO)
import Utilities                    (wrapValidator, writeValidatorToFile)

data VestingDatum = VestingDatum {
    beneficiary :: PubKeyHash,
    deadline :: POSIXTime
}
unstableMakeIsData ''VestingDatum

{-# INLINABLE validator #-}
validator :: VestingDatum -> () -> ScriptContext -> Bool 
validator dtm _ ctx = traceIfFalse "Can only claim after deadline" deadlinePassed &&
                        traceIfFalse "Not allowed to claim" isBeneficiary
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx

            deadlinePassed :: Bool
            deadlinePassed = contains (from $ deadline dtm) (txInfoValidRange txInfo)

            isBeneficiary :: Bool
            isBeneficiary = txSignedBy txInfo $ beneficiary dtm


{-# INLINABLE wrappedVal #-}
wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator validator

compiledValidator :: Validator
compiledValidator = mkValidatorScript $$(compile [|| wrappedVal ||])

writeToFile :: IO ()
writeToFile = writeValidatorToFile "./assets/vesting.plutus" compiledValidator