{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module ZingCoinParameterized where

import Plutus.V2.Ledger.Api      (TxInfo, PubKeyHash, TokenName , BuiltinData, UnsafeFromData (unsafeFromBuiltinData))
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy, ScriptContext, TxInfo (txInfoMint))
import PlutusTx.Prelude          (Bool, traceIfFalse, (==))
import PlutusTx                  (compile, unstableMakeIsData, CompiledCode)
import Plutus.V1.Ledger.Value    (flattenValue)
import Utilities                 (wrapPolicy, writeCodeToFile)
import Prelude                   ((&&), ($), Bool (False), IO)

data CoinParams = CoinParams {
    owner :: PubKeyHash,
    tokenName :: TokenName
}
unstableMakeIsData ''CoinParams

{-# INLINABLE policy #-}
policy :: CoinParams -> () -> ScriptContext -> Bool
policy p _ ctx = traceIfFalse "Unauthorized minting" signedByOwner  &&
                 traceIfFalse "Invalid tokens"       tokensValid
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        signedByOwner :: Bool
        signedByOwner = txSignedBy txInfo $ owner p

        tokensValid :: Bool
        tokensValid = case flattenValue $ txInfoMint txInfo of 
                [(_, tn, _)] -> tn == tokenName p
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

compiledPolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingcoin.plutus" compiledPolicyCode