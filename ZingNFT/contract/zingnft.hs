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
import PlutusTx.Prelude         (traceIfFalse, find, Integer, (==), map, Ord ((<=)), (+), (&&),
                                 MultiplicativeSemigroup ((*)), isJust, encodeUtf8, divMod, otherwise, foldr, (++), BuiltinString)
import PlutusTx.Builtins        (appendString, equalsInteger)
import PlutusTx.Builtins.Class  (stringToBuiltinString)
import Prelude                  (Bool (False), Maybe (Just, Nothing), IO, (.), ($), Semigroup ((<>)), Char)
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
            
            expectedId :: Integer
            expectedId = case returnedThreadDatum of
                Just d -> (threadIdx d * threadCount d) + mintCount d
                _      -> 0 -- fix

            idCorrect :: Bool
            idCorrect = case flattenValue $ txInfoMint txInfo of  
                [(_, tn, _)] -> unTokenName tn == tokenPrefix info <> integerToBuiltinByteString expectedId
                -- [(_, tn, _)] -> True
                _            -> False

{-# INLINABLE wrappedPolicy #-}
wrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedPolicy p = wrapPolicy (policy $ unsafeFromBuiltinData p)

{-# INLINABLE compiledPolicyCode #-}
compiledPolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledPolicyCode = $$(compile [|| wrappedPolicy ||])

saveCode :: IO ()
saveCode = writeCodeToFile "./assets/zingnft.plutus" compiledPolicyCode

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString i = encodeUtf8 $ intToString i

{-# INLINEABLE intToString #-}
intToString :: Integer -> BuiltinString
intToString i = foldr appendString "" strings
  where
    ints = intToInts i
    strings = map (charToString . intToChar) ints

charToString :: Char -> BuiltinString
charToString c = stringToBuiltinString [c]

{-# INLINEABLE intToInts #-}
intToInts :: Integer -> [Integer]
intToInts i
  | equalsInteger a 0 = [b]
  | otherwise = intToInts a ++ [b]
  where
    (a, b) = divMod i 10

{-# INLINEABLE intToChar #-}
intToChar :: Integer -> Char
intToChar i
  | equalsInteger i 0 = '0'
  | equalsInteger i 1 = '1'
  | equalsInteger i 2 = '2'
  | equalsInteger i 3 = '3'
  | equalsInteger i 4 = '4'
  | equalsInteger i 5 = '5'
  | equalsInteger i 6 = '6'
  | equalsInteger i 7 = '7'
  | equalsInteger i 8 = '8'
  | equalsInteger i 9 = '9'
  | otherwise = '0' -- Fix this