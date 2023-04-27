{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module SpendAny where

import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), Validator, mkValidatorScript, POSIXTime (POSIXTime), TxInfo (txInfoValidRange))
import Utilities            (wrapValidator, writeValidatorToFile)
import Prelude              (IO)
import PlutusTx             (compile)
import PlutusTx.Prelude
import Plutus.V1.Ledger.Interval (contains, to)

validator :: () -> () -> ScriptContext -> Bool
validator _ _ ctx = traceIfFalse "Can't spend after deadline" beforeDead
        where 
            beforeDead :: Bool
            beforeDead = contains (to $ POSIXTime 1714116796000) (txInfoValidRange $ scriptContextTxInfo ctx)

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator validator

compiledValidator :: Validator 
compiledValidator = mkValidatorScript $$(compile [|| wrappedVal ||])

writeToFile :: IO ()
writeToFile = writeValidatorToFile "./assets/spendany.plutus" compiledValidator