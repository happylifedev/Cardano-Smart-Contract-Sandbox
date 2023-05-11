{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZingNFT where

import Plutus.V2.Ledger.Api     (ScriptContext (scriptContextTxInfo), CurrencySymbol, BuiltinByteString, BuiltinData,
                                TxInfo (txInfoInputs, txInfoData, txInfoOutputs, txInfoMint),
                                UnsafeFromData (unsafeFromBuiltinData), TxInInfo (txInInfoOutRef, txInInfoResolved),
                                TxOut (txOutValue, txOutDatum), TokenName (TokenName, unTokenName),
                                OutputDatum (OutputDatum), singleton, Value, Datum (getDatum))
import Plutus.V1.Ledger.Value   (flattenValue)
import PlutusTx                 (unstableMakeIsData, compile, CompiledCode)
import PlutusTx.Prelude         (traceIfFalse, find, Integer, (==), map, Ord ((<=), (<), (>=)), (+), (&&),
                                 MultiplicativeSemigroup ((*)), isJust, appendByteString, consByteString)
import Prelude                  (Bool (False), Maybe (Just, Nothing), IO, ($))
import Utilities                (wrapPolicy, writeCodeToFile)

data ContractInfo = ContractInfo {
    threadToken :: CurrencySymbol,
    maxSupply :: Integer,
    tokenPrefix :: BuiltinByteString
}
unstableMakeIsData ''ContractInfo

data ThreadDatum = ThreadDatum {
    mintCount :: Integer,
    threadIdx :: Integer,
    threadCount :: Integer
}
unstableMakeIsData ''ThreadDatum

{-# INLINABLE policy #-}
policy :: ContractInfo -> () -> ScriptContext -> Bool
policy info _ ctx = traceIfFalse "Doesn't consume a threadtoken"             consumesThread &&
                    traceIfFalse "Returned thread either missing or invalid" returnsThread  &&
                    traceIfFalse "Max supply reached"                        belowSupply    &&
                    traceIfFalse "Token ID not correct"                      idCorrect

        where 
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx
            
            threadValue :: Value
            threadValue = singleton (threadToken info) (TokenName "Thread") 1

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
            consumesThread = isJust consumedThreadDatum
            
            returnsThread :: Bool
            returnsThread = case returnedThreadDatum of
              Just rd -> case consumedThreadDatum of
                    Just cd -> mintCount rd == mintCount cd + 1
                    _       ->  False
              Nothing -> False

            belowSupply :: Bool
            belowSupply = case returnedThreadDatum of
                Just d -> mintCount d <= maxSupply info
                _       -> False
            
            threadedId :: Integer
            threadedId = case returnedThreadDatum of
                Just d -> (threadIdx d * threadCount d) + mintCount d
                _      -> -1

            idCorrect :: Bool
            idCorrect = case flattenValue $ txInfoMint txInfo of
                [(_, tn, _)] -> unTokenName tn == appendByteString (tokenPrefix info) (consByteString (48 + threadedId) "") && 
                                threadedId < 10 &&
                                threadedId >= 0
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

{-# INLINABLE compiledPolicyCode #-}
compiledPolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

saveCode :: IO ()
saveCode = writeCodeToFile "./assets/zingnft.plutus" compiledPolicyCode