{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module ZingCoin where

import Plutus.V2.Ledger.Api      (TxInfo, PubKeyHash, TokenName , BuiltinData, MintingPolicy, mkMintingPolicyScript)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy, ScriptContext, TxInfo (txInfoMint))
import PlutusTx.Prelude          (Bool, traceIfFalse, (==))
import PlutusTx                  (compile, liftCode, makeLift, applyCode)
import Plutus.V1.Ledger.Value    (flattenValue)
import Utilities                 (wrapPolicy, writePolicyToFile)
import Prelude                   ((&&), ($), Bool (False), IO, (.))

data CoinParams = CoinParams {
    owner :: PubKeyHash,
    tokenName :: TokenName
}
makeLift ''CoinParams

zingCoinParams :: CoinParams
zingCoinParams = CoinParams {
    owner = "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83",
    tokenName = "ZingCoinV2"
}

-- ZingCoinV2
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
wrappedPolicy :: CoinParams -> BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy . policy

compiledPolicy :: CoinParams -> MintingPolicy
compiledPolicy p = mkMintingPolicyScript ($$(compile [|| wrappedPolicy ||]) `applyCode` liftCode p)

writeToFile :: IO ()
writeToFile = writePolicyToFile "./assets/zingcoin.plutus" $ compiledPolicy zingCoinParams

-- ZingCoinV1
-- {-# INLINABLE policy #-}
-- policy :: PubKeyHash -> () -> ScriptContext -> Bool
-- policy pkh _ ctx = traceIfFalse "Unauthorized minting" signedByOwner  &&
--                traceIfFalse "Invalid tokens"       tokensValid
--     where
--         txInfo :: TxInfo
--         txInfo = scriptContextTxInfo ctx

--         signedByOwner :: Bool
--         signedByOwner = txSignedBy txInfo pkh

--         tokensValid :: Bool
--         tokensValid = case flattenValue $ txInfoMint txInfo of 
--                 [(_, tn, _)] -> tn == TokenName "ZingCoin"
--                 _            -> False

-- {-# INLINABLE wrappedPolicy #-}
-- wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrappedPolicy pkh = wrapPolicy (policy $ unsafeFromBuiltinData pkh)

-- {-# INLINABLE compiledPolicy #-}
-- compiledPolicy :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- compiledPolicy = $$(compile [|| wrappedPolicy ||])

-- writeToFile :: IO ()
-- writeToFile = writeCodeToFile "./assets/zingcoin.plutus" compiledPolicy