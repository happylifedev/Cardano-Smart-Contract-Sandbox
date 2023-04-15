{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module ZingCoin where

import Plutus.V2.Ledger.Api      (TxInfo, PubKeyHash (PubKeyHash), TokenName (..), BuiltinData)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy, ScriptContext, TxInfo (txInfoMint))
import PlutusTx.Prelude          (Bool, traceIfFalse, (==))
import PlutusTx                  ( UnsafeFromData (unsafeFromBuiltinData), CompiledCode, compile)
import Plutus.V1.Ledger.Value    (flattenValue)
import Utilities                 (wrapPolicy, writeCodeToFile)
import Prelude                   ((&&), ($), Bool (False), IO)
import Data.String               (fromString)

-- data ZingCoinParams = ZingCoinParams {
--     owner :: PubKeyHash,
--     tokenName :: TokenName
-- }

-- instance UnsafeFromData ZingCoinParams
-- policy :: ZingCoinParams -> () -> ScriptContext -> Bool


{-# INLINABLE policy #-}
policy :: () -> ScriptContext -> Bool
policy _ ctx = traceIfFalse "Unauthorized minting" signedByOwner  &&
               traceIfFalse "Invalid tokens"       tokensValid
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        signedByOwner :: Bool
        signedByOwner = txSignedBy txInfo $ PubKeyHash "7d3c8f8cdc2dc42924f3cf8fa2f72bf936531cc3ebf56340a29f9036"

        tokensValid :: Bool
        tokensValid = case flattenValue $ txInfoMint txInfo of 
                [(_, tn, _)] -> tn == TokenName "ZingCoin"
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy policy

{-# INLINABLE compiledPolicy #-}
compiledPolicy :: CompiledCode (BuiltinData -> BuiltinData -> ())
compiledPolicy = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingcoin.plutus" compiledPolicy