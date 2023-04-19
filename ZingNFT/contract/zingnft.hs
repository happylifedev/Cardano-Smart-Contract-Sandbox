{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}

module ZingNFT where

import Plutus.V2.Ledger.Api      (PubKeyHash (PubKeyHash), POSIXTime (POSIXTime), ScriptContext (scriptContextTxInfo),
                                  BuiltinData, mkMintingPolicyScript, MintingPolicy, BuiltinByteString, 
                                  TxInfo (txInfoValidRange, txInfoMint), TokenName (TokenName)
                                 )
import Prelude                   (Bool (True, False), (.), IO, (&&), ($), Eq ((==)))
import Utilities                 (wrapPolicy, writePolicyToFile)
import PlutusTx                  (makeLift, compile, liftCode, applyCode)
import PlutusTx.Prelude          (traceIfFalse)
import Data.Aeson (Value(Bool))
import Plutus.V1.Ledger.Interval (to, contains)
import Plutus.V2.Ledger.Contexts (txSignedBy, txOutValue, txInfoOutputs)
import Plutus.V1.Ledger.Value    (flattenValue)
import Data.List

data Params = Params {
    owner :: PubKeyHash,
    tokenPrefix :: TokenName,
    lockAfter :: POSIXTime
}
makeLift ''Params

zingNFTParams :: Params
zingNFTParams = Params {
    owner = PubKeyHash "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83",
    tokenPrefix = TokenName "ZingBoi",
    lockAfter = POSIXTime 1713550450
}

policy :: Params -> () -> ScriptContext -> Bool
policy p r ctx = traceIfFalse "Can't mint after deadline" beforeLock &&
                 traceIfFalse "Unauthorized to mint" ownerApproved &&
                 traceIfFalse "Token details incorrect" tokenCorrect
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx

            beforeLock :: Bool
            beforeLock = contains (to $ lockAfter p) (txInfoValidRange txInfo)

            ownerApproved :: Bool
            ownerApproved = txSignedBy txInfo $ owner p

            tokenCorrect :: Bool
            tokenCorrect = case flattenValue $ txInfoMint txInfo of 
                -- [(_, tn, _)] -> take 7 tn == tokenPrefix p 
                [(_, tn, _)] -> take 7 tn == tokenPrefix p 
                _            -> False


wrappedPolicy :: Params -> BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy . policy

compiledPolicy :: Params -> MintingPolicy
compiledPolicy p = mkMintingPolicyScript ($$(compile [|| wrappedPolicy||]) `applyCode` liftCode p)

writeToFile :: IO ()
writeToFile = writePolicyToFile "./assets/zingNFT.plutus" (compiledPolicy zingNFTParams)