{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module ZingCoin where

import Plutus.V2.Ledger.Api      (TxInfo, PubKeyHash, TokenName (..), BuiltinData, unsafeFromBuiltinData)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy, ScriptContext, TxInfo (txInfoMint))
import PlutusTx.Prelude          (Bool, traceIfFalse, (==))
import PlutusTx                  (CompiledCode, compile)
import Plutus.V1.Ledger.Value    (flattenValue)
import Utilities                 (wrapPolicy, writeCodeToFile)
import Prelude                   ((&&), ($), Bool (False), IO)

-- data ZingCoinParams = ZingCoinParams {
--     owner :: PubKeyHash,
--     tokenName :: TokenName
-- }

-- instance UnsafeFromData ZingCoinParams
-- policy :: ZingCoinParams -> () -> ScriptContext -> Bool


{-# INLINABLE policy #-}
policy :: PubKeyHash -> () -> ScriptContext -> Bool
policy pkh _ ctx = traceIfFalse "Unauthorized minting" signedByOwner  &&
               traceIfFalse "Invalid tokens"       tokensValid
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        signedByOwner :: Bool
        signedByOwner = txSignedBy txInfo pkh

        tokensValid :: Bool
        tokensValid = case flattenValue $ txInfoMint txInfo of 
                [(_, tn, _)] -> tn == TokenName "ZingCoin"
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy pkh = wrapPolicy (policy $ unsafeFromBuiltinData pkh)

{-# INLINABLE compiledPolicy #-}
compiledPolicy :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicy = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingcoin.plutus" compiledPolicy