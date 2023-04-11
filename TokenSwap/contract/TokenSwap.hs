{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module TokenSwap where

import           Plutus.V2.Ledger.Api      (
                                           ScriptContext (scriptContextTxInfo), PubKeyHash, Validator,
                                           mkValidatorScript, adaToken, adaSymbol, singleton,
                                           TxInInfo (txInInfoResolved),
                                           TxInfo (txInfoInputs, txInfoOutputs),
                                           TxOut (txOutAddress), Address (addressCredential), Credential (PubKeyCredential)
                                           )
import           Plutus.V2.Ledger.Contexts (valuePaidTo, ownHash)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Builtins         (BuiltinData, Integer)
import           PlutusTx.Prelude          (Bool (..), (==), traceIfFalse, (&&), length, filter, map)
import           Plutus.V1.Ledger.Address  (scriptHashAddress)
import           Utilities                 (wrapValidator, writeValidatorToFile)
import           Prelude                   (IO, (||), all)


data DatumSwap = DatumSwap { seller :: PubKeyHash
                           , price  :: Integer
                           }
PlutusTx.unstableMakeIsData ''DatumSwap

{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx = (traceIfFalse "You have to pay the seller!" outputToSeller &&
                        traceIfFalse "Can only consume one utxo at a time" singleOutputConsumed)
                       || consumerIsSeller
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        outputToSeller :: Bool
        outputToSeller =
          valuePaidTo txInfo (seller ds) == singleton adaSymbol adaToken (price ds)
        
        singleOutputConsumed :: Bool
        singleOutputConsumed = length (filter (\x -> txOutAddress x == scriptHashAddress (ownHash ctx)) (map txInInfoResolved (txInfoInputs txInfo))) == 1

        consumerIsSeller :: Bool
        consumerIsSeller = all (\o -> addressCredential (txOutAddress o) == PubKeyCredential (seller ds)) (txInfoOutputs txInfo)

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])

----- helpers
writeValidator :: IO ()
writeValidator = writeValidatorToFile "./assets/tokenswap.plutus" validator