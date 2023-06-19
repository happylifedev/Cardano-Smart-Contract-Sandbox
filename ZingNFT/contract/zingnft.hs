{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZingNFT where

import Plutus.V2.Ledger.Api       (ScriptContext (scriptContextTxInfo), CurrencySymbol, BuiltinData,
                                  TxInfo (txInfoInputs),
                                  UnsafeFromData (unsafeFromBuiltinData), TxInInfo (txInInfoResolved),
                                  TxOut (txOutValue), TokenName (TokenName), singleton, 
                                  Value, MintingPolicy, mkMintingPolicyScript, ToData (toBuiltinData))
import PlutusTx                   (unstableMakeIsData, compile, CompiledCode, applyCode, makeLift, liftCode)
import PlutusTx.Prelude           (traceIfFalse, find, (==), map, isJust, Bool)
import Prelude                    (IO, ($), Show)
import Utilities                  (wrapPolicy, writeCodeToFile, writePolicyToFile)

data ContractInfo = ContractInfo {
    threadToken :: CurrencySymbol
} deriving Show
unstableMakeIsData ''ContractInfo
makeLift ''ContractInfo

{-# INLINABLE policy #-}
policy :: ContractInfo -> () -> ScriptContext -> Bool
policy info _ ctx = traceIfFalse "Doesn't consume a threadtoken" consumesThread
        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx
            
            threadValue :: Value
            threadValue = singleton (threadToken info) (TokenName "thread") 1

            consumesThread :: Bool
            consumesThread = isJust $ find (\x -> txOutValue x == threadValue) (map txInInfoResolved $ txInfoInputs txInfo)

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

{-# INLINABLE compiledPolicyCode #-}
compiledPolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingnft.plutus" compiledPolicyCode

-- For testing
compiledPolicy :: ContractInfo -> MintingPolicy
compiledPolicy i = mkMintingPolicyScript ($$(compile [|| wrappedPolicy ||]) `applyCode` liftCode (toBuiltinData i))

writeTestPolicyToFile :: ContractInfo -> IO ()
writeTestPolicyToFile i = writePolicyToFile "./assets/zingnft.plutus" (compiledPolicy i)