{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module ZingNFT_TimeLocked where

import Plutus.V2.Ledger.Api      (
                                  PubKeyHash, POSIXTime (POSIXTime), ScriptContext (scriptContextTxInfo),
                                  BuiltinData, mkMintingPolicyScript, MintingPolicy,
                                  BuiltinByteString,
                                  TxInfo (txInfoValidRange, txInfoMint), TokenName (unTokenName),
                                 )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Interval (to, contains)
import Plutus.V1.Ledger.Value    (flattenValue)
import PlutusTx.Prelude          (traceIfFalse, (==), sliceByteString, lengthOfByteString)
import PlutusTx                  (makeLift, compile, liftCode, applyCode)
import Utilities                 (wrapPolicy, writePolicyToFile)
import Prelude                   (Bool (False), (.), IO, (&&), ($))

data Constants = Constants {
    owner :: PubKeyHash,
    tokenPrefix :: BuiltinByteString,
    lock :: POSIXTime
}
makeLift ''Constants

zingNFTConstants :: Constants
zingNFTConstants = Constants {
    owner = "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83",
    tokenPrefix = "ZingBoi",
    lock = POSIXTime 1714116796000
}

{-# INLINABLE policy #-}
policy :: Constants -> () -> ScriptContext -> Bool
policy p _ ctx = traceIfFalse "Unauthorized to mint" ownerApproved &&
                 traceIfFalse "Can't mint after deadline" beforeLock &&
                 traceIfFalse "Token details incorrect" tokenCorrect
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx

            beforeLock :: Bool
            beforeLock = contains (to $ lock p) (txInfoValidRange txInfo)

            ownerApproved :: Bool
            ownerApproved = txSignedBy txInfo $ owner p

            tokenCorrect :: Bool
            tokenCorrect = case flattenValue $ txInfoMint txInfo of 
                [(_, tn, _)] -> sliceByteString 0 prefixLength (unTokenName tn) == tokenPrefix p
                    where
                        prefixLength = lengthOfByteString $ tokenPrefix p
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: Constants -> BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy . policy

{-# INLINABLE compiledPolicy #-}
compiledPolicy :: Constants -> MintingPolicy
compiledPolicy p = mkMintingPolicyScript ($$(compile [|| wrappedPolicy ||]) `applyCode` liftCode p)

writeToFile :: IO ()
writeToFile = writePolicyToFile "./assets/zingNFT.plutus" (compiledPolicy zingNFTConstants)