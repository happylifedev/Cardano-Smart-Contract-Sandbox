 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module ZingNFT where

import Plutus.V2.Ledger.Api (ScriptContext (scriptContextTxInfo), CurrencySymbol, BuiltinByteString, BuiltinData,
                            TxInfo (txInfoInputs, txInfoData, txInfoOutputs, txInfoMint),UnsafeFromData (unsafeFromBuiltinData),
                            TxInInfo (txInInfoOutRef, txInInfoResolved),TxOut (txOutValue, txOutDatum), TokenName (TokenName, unTokenName),
                            Value (Value, getValue), fromList, OutputDatum (OutputDatum, OutputDatumHash), Datum (getDatum, Datum), singleton, toData, ToData (toBuiltinData))
import PlutusTx             (unstableMakeIsData, compile, CompiledCode)
import Utilities            (wrapPolicy, writeCodeToFile)
import PlutusTx.Prelude     (traceIfFalse, find, any, Integer, (==), map, Ord ((>=), (<=)), (+), (&&), MultiplicativeSemigroup ((*)), isJust, encodeUtf8)
import Prelude              (lookup, Bool (False, True), Maybe (Just, Nothing), IO, (.), ($), Semigroup ((<>)), Show (show))
import Plutus.V1.Ledger.Value (flattenValue)
import PlutusTx.Builtins (serialiseData)
import qualified Data.ByteString.Char8


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
                    traceIfFalse "Returned thread either missing or invalid" returnsThread &&
                    traceIfFalse "Max supply reached"                        belowSupply
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
            
            correctId :: Integer
            correctId = case returnedThreadDatum of
                Just d -> (threadIdx d * threadCount d) + mintCount d
                _      -> 0

            idCorrect :: Bool
            idCorrect = case flattenValue $ txInfoMint txInfo of 
                -- [(_, tn, _)] -> unTokenName tn == tokenPrefix info <> serialiseData (toBuiltinData correctId)
                [(_, tn, _)] -> unTokenName tn == tokenPrefix info <> show correctId
--               ^^                    
--              currencySymbol is known to be correct since this contract is running
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

{-# INLINABLE compiledPolicy #-}
compiledPolicy :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicy = $$(compile [|| wrappedPolicy ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/zingnft.plutus" compiledPolicy