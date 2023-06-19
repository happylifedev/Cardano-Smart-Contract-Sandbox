{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ThreadVal where

import Plutus.V2.Ledger.Api       (ScriptContext (scriptContextTxInfo), OutputDatum (OutputDatum),
                                  UnsafeFromData (unsafeFromBuiltinData), Datum (getDatum), 
                                  TxOut (txOutDatum, txOutValue),BuiltinByteString, CurrencySymbol, Value, 
                                  TxInfo (txInfoOutputs, txInfoMint), TokenName (TokenName), singleton, txInfoOutputs, 
                                  BuiltinData)
import PlutusTx.Prelude           (traceIfFalse, find, BuiltinString, encodeUtf8, appendString, appendByteString, (==),
                                  (+), (<), map, (&&), (+), Ord((<)), Integer, map, foldr, (++), divMod, otherwise)
import PlutusTx                   (unstableMakeIsData, CompiledCode, compile)
import PlutusTx.Builtins          (equalsInteger)
import Plutus.V1.Ledger.Value     (valueOf)
import Utilities                  (wrapValidator, writeCodeToFile)
import Prelude                    (Show, Maybe (Just, Nothing), Bool (False), IO, ($))

data ThreadDatum = ThreadDatum {
    mintCount :: Integer,
    threadIdx :: Integer,   
    threadCount :: Integer
} deriving Show
unstableMakeIsData ''ThreadDatum

data ThreadInfo = ThreadInfo {
    threadToken :: CurrencySymbol, 
    tokenPolicy :: CurrencySymbol,
    tokenPrefix :: BuiltinByteString,
    maxSupply :: Integer
} 
unstableMakeIsData ''ThreadInfo

{-# INLINABLE threadValidator #-}
threadValidator :: ThreadInfo -> ThreadDatum -> () -> ScriptContext -> Bool
threadValidator info dtm _ ctx = 
        traceIfFalse "Thread token either not returned or invalid"      returnsThread &&
        traceIfFalse "Max supply reached"                               belowSupply &&
        traceIfFalse "Associated NFT mint is either missing or invalid" mintsNFT
    where

        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        threadValue :: Value
        threadValue = singleton (threadToken info) (TokenName "thread") 1

        returnedThreadDatum :: Maybe ThreadDatum
        returnedThreadDatum = case find (\x -> txOutValue x PlutusTx.Prelude.== threadValue) (txInfoOutputs txInfo) of 
            Just out -> case txOutDatum out of 
               OutputDatum d -> unsafeFromBuiltinData $ getDatum d
               _           -> Nothing
            _              -> Nothing

        returnsThread :: Bool
        returnsThread = case returnedThreadDatum of
            Just rd ->
                mintCount rd   PlutusTx.Prelude.== mintCount dtm PlutusTx.Prelude.+ 1 && -- 1 mint per Tx
                threadIdx rd   PlutusTx.Prelude.== threadIdx dtm     &&
                threadCount rd PlutusTx.Prelude.== threadCount dtm 
            _       -> False
        
        belowSupply :: Bool
        belowSupply = mintCount dtm PlutusTx.Prelude.< maxSupply info

        mintsNFT :: Bool -- enforce 1 per Tx
        mintsNFT = valueOf (txInfoMint txInfo) (tokenPolicy info) 
            (TokenName (appendByteString (tokenPrefix info) (intToBuiltinByteString $ mintCount dtm PlutusTx.Prelude.+ 1 ))) PlutusTx.Prelude.== 1

{-# INLINABLE wrappedThreadValidator #-}
wrappedThreadValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedThreadValidator p = wrapValidator (threadValidator $ unsafeFromBuiltinData p)

{-# INLINABLE threadValidatorCode #-}
threadValidatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
threadValidatorCode = $$(compile [|| wrappedThreadValidator ||])

writeToFile :: IO ()
writeToFile = writeCodeToFile "./assets/threadval.plutus" threadValidatorCode

--------------------------------------------------
-- helper
--------------------------------------------------

{-# INLINEABLE intToBuiltinByteString #-}
intToBuiltinByteString :: Integer -> BuiltinByteString
intToBuiltinByteString i = encodeUtf8 $ intToString i

{-# INLINEABLE intToString #-}
intToString :: Integer -> BuiltinString
intToString i = foldr appendString "" strings
  where
    ints = intToInts i
    strings = PlutusTx.Prelude.map intToChar ints

{-# INLINEABLE intToInts #-}
intToInts :: Integer -> [Integer]
intToInts i
  | equalsInteger a 0 = [b]
  | otherwise = intToInts a ++ [b]
  where
    (a, b) = divMod i 10

{-# INLINEABLE intToChar #-}
intToChar :: Integer -> BuiltinString
intToChar i
  | equalsInteger i 0 = "0"
  | equalsInteger i 1 = "1"
  | equalsInteger i 2 = "2"
  | equalsInteger i 3 = "3"
  | equalsInteger i 4 = "4"
  | equalsInteger i 5 = "5"
  | equalsInteger i 6 = "6"
  | equalsInteger i 7 = "7"
  | equalsInteger i 8 = "8"
  | equalsInteger i 9 = "9"
  | otherwise = "0" -- fix this; unsafe if passed a 2 digit number, wouldn't happen in this implementation