{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}

module ZingNFT where

import Plutus.V2.Ledger.Api      (
                                  PubKeyHash (PubKeyHash), POSIXTime (POSIXTime), ScriptContext (scriptContextTxInfo),
                                  BuiltinData, mkMintingPolicyScript, MintingPolicy, 
                                  TxInfo (txInfoValidRange, txInfoMint), TokenName (TokenName)
                                 )
import Prelude                   (Bool (False), (.), IO, (&&), ($), Eq ((==)))
import Utilities                 (wrapPolicy, writePolicyToFile)
import PlutusTx                  (makeLift, compile, liftCode, applyCode)
import PlutusTx.Prelude          (traceIfFalse)
import Plutus.V1.Ledger.Interval (to, contains)
import Plutus.V1.Ledger.Value    (flattenValue, toString)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Data.List

data Constants = Constants {
    owner :: PubKeyHash,
    tokenPrefix :: TokenName,
    lockAfter :: POSIXTime
}
makeLift ''Constants

zingNFTConstants :: Constants
zingNFTConstants = Constants {
    owner = PubKeyHash "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83",
    tokenPrefix = TokenName "ZingBoi",
    lockAfter = POSIXTime 1713550450
}

policy :: Constants -> () -> ScriptContext -> Bool
policy p _ ctx = traceIfFalse "Can't mint after deadline" beforeLock &&
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
                [(_, tn, _)] -> take prefixLength (toString tn) == toString (tokenPrefix p)
                    where
                        prefixLength = length $ toString $ tokenPrefix p
                _            -> False

wrappedPolicy :: Constants -> BuiltinData -> BuiltinData -> ()
wrappedPolicy = wrapPolicy . policy

compiledPolicy :: Constants -> MintingPolicy
compiledPolicy p = mkMintingPolicyScript ($$(compile [|| wrappedPolicy||]) `applyCode` liftCode p)

writeToFile :: IO ()
writeToFile = writePolicyToFile "./assets/zingNFT.plutus" (compiledPolicy zingNFTConstants)