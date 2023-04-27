{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module ZingNFT_Thread where

import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), TxOutRef, TxInfo, txInInfoOutRef, txInfoInputs, BuiltinData, UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude     (traceIfFalse, find, map)
import Prelude              (Bool (True, False), Maybe (Nothing, Just), Eq ((==)), ($), IO)
import Utilities            (wrapPolicy, writeCodeToFile)
import PlutusTx             (compile, CompiledCode)

{-# INLINABLE policy #-}
policy :: TxOutRef -> () -> ScriptContext -> Bool
policy ref _ ctx = traceIfFalse "Expected output was not consumed" consumesInput
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        consumesInput :: Bool
        consumesInput = case find (== ref) (map txInInfoOutRef (txInfoInputs txInfo)) of
            Just _   -> True
            Nothing  -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

compiledPolicyCode :: CompiledCode(BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingnft_thread.plutus" compiledPolicyCode