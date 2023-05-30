{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Thread where

import Plutus.V2.Ledger.Api   (ScriptContext (scriptContextTxInfo), TxOutRef, TxInfo (txInfoMint), txInInfoOutRef, txInfoInputs, BuiltinData, UnsafeFromData (unsafeFromBuiltinData), mkMintingPolicyScript, ToData (toBuiltinData), MintingPolicy)
import PlutusTx.Prelude       (traceIfFalse, find, map, (&&))
import Prelude                (Bool (True, False), Maybe (Nothing, Just), Eq ((==)), ($), IO)
import Utilities              (wrapPolicy, writeCodeToFile)
import PlutusTx               (compile, CompiledCode, applyCode, liftCode)
import Plutus.V1.Ledger.Value (flattenValue)

{-# INLINABLE policy #-}
policy :: TxOutRef -> () -> ScriptContext -> Bool
policy ref _ ctx = traceIfFalse "Expected output was not consumed" consumesInput &&
                   traceIfFalse "TokenName incorrect"              correctName
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        consumesInput :: Bool
        consumesInput = case find (== ref) (map txInInfoOutRef (txInfoInputs txInfo)) of
            Just _   -> True
            Nothing  -> False
        
        correctName :: Bool
        correctName = case flattenValue $ txInfoMint txInfo of
            [(_, tn, _)] -> tn == "thread"
            _          -> False


{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

compiledPolicyCode :: CompiledCode(BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

compiledPolicy :: TxOutRef -> MintingPolicy 
compiledPolicy out = mkMintingPolicyScript ($$(compile [|| wrappedPolicy ||]) `applyCode` liftCode (toBuiltinData out)) 

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingnft_thread.plutus" compiledPolicyCode