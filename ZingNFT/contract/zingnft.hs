{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module ZingNFT where

import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), CurrencySymbol, BuiltinByteString, BuiltinData,
                            TxInfo (txInfoInputs, txInfoData, txInfoOutputs),UnsafeFromData (unsafeFromBuiltinData),
                            TxInInfo (txInInfoOutRef, txInInfoResolved),TxOut (txOutValue, txOutDatum), TokenName (TokenName),
                            Value (Value, getValue), fromList, OutputDatum (OutputDatum, OutputDatumHash), Datum (getDatum, Datum))
import PlutusTx             (unstableMakeIsData, compile, CompiledCode)
import Utilities            (wrapPolicy, writeCodeToFile)
import PlutusTx.Prelude     (traceIfFalse, find, any, Integer, (==), map, Ord ((>=)), (+), (&&))
import Prelude              (lookup, Bool (False, True), Maybe (Just, Nothing), IO, (.), ($))


data ContractInfo = ContractInfo {
    threadToken :: CurrencySymbol,
    maxSupply :: Integer,
    tokenPrefix :: BuiltinByteString
}
unstableMakeIsData ''ContractInfo

data ThreadDatum = ThreadDatum {
    mintCount :: Integer,
    threadIdx :: Integer
}
unstableMakeIsData ''ThreadDatum

{-# INLINABLE policy #-}
policy :: ContractInfo -> () -> ScriptContext -> Bool
policy info _ ctx = traceIfFalse "Doesn't consume a threadtoken"             consumesThread &&
                      traceIfFalse "Returned thread either missing or invalid" returnsThread
        where 
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx
            
            threadValue :: Value
            threadValue = Value $ fromList [(threadToken info, fromList [(TokenName "Thread", 1)])]

            consumedThreadDatum :: Maybe ThreadDatum
            consumedThreadDatum = case find (\x -> txOutValue x == threadValue) (map txInInfoResolved $ txInfoInputs txInfo) of 
                 Just out -> case txOutDatum out of 
                    OutputDatum d -> unsafeFromBuiltinData $ getDatum d
                    _           -> Nothing
                 _              -> Nothing

            returnedThreadDatum :: Maybe ThreadDatum
            returnedThreadDatum = case find (\x -> txOutValue x == threadValue) (txInfoOutputs txInfo) of 
                 Just out -> case txOutDatum out of 
                    OutputDatum d -> unsafeFromBuiltinData $ getDatum d
                    _           -> Nothing
                 _              -> Nothing

            consumesThread :: Bool
            consumesThread = case consumedThreadDatum of
              Just _  -> True
              Nothing -> False
            
            returnsThread :: Bool
            returnsThread = case returnedThreadDatum of
              Just rd -> case consumedThreadDatum of 
                    Just cd -> mintCount rd == mintCount cd + 1
                    _       -> False
              Nothing -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

{-# INLINABLE compiledPolicy #-}
compiledPolicy :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicy = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingnft.plutus" compiledPolicy