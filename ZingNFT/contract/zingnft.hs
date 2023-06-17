{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZingNFT where

import Plutus.V2.Ledger.Api       (ScriptContext (scriptContextTxInfo), CurrencySymbol, BuiltinByteString, BuiltinData,
                                  TxInfo (txInfoInputs, txInfoOutputs, txInfoMint),
                                  UnsafeFromData (unsafeFromBuiltinData), TxInInfo (txInInfoResolved),
                                  TxOut (txOutValue, txOutDatum), TokenName (TokenName, unTokenName),
                                  OutputDatum (OutputDatum), singleton, Value, Datum (getDatum), MintingPolicy, mkMintingPolicyScript, ToData (toBuiltinData))
import Plutus.V1.Ledger.Value     (flattenValue)
import PlutusTx                   (unstableMakeIsData, compile, CompiledCode, applyCode, makeLift, liftCode)
import PlutusTx.Prelude           (traceIfFalse, find, Integer, (==), map, Ord ((<=), (<), (>=)), (+), (&&),
                                   MultiplicativeSemigroup ((*)), isJust, appendByteString, otherwise, divMod, (++), foldr, appendString, encodeUtf8, BuiltinString)
import Prelude                    (Bool (False), Maybe (Just, Nothing), IO, ($), Show)
import Utilities                  (wrapPolicy, writeCodeToFile, writePolicyToFile)
import PlutusTx.Builtins          (equalsInteger)

data ContractInfo = ContractInfo {
    threadToken :: CurrencySymbol,
    maxSupply :: Integer,
    tokenPrefix :: BuiltinByteString
} deriving Show
unstableMakeIsData ''ContractInfo
makeLift ''ContractInfo

data ThreadDatum = ThreadDatum {
    mintCount :: Integer,
    threadIdx :: Integer,
    threadCount :: Integer
} deriving Show
unstableMakeIsData ''ThreadDatum

{-# INLINABLE policy #-}
policy :: ContractInfo -> () -> ScriptContext -> Bool
policy info _ ctx = traceIfFalse "Doesn't consume a threadtoken"             consumesThread &&
                    traceIfFalse "Returned thread either missing or invalid" returnsThread  &&
                    traceIfFalse "Max supply reached"                        belowSupply    &&
                    traceIfFalse "TokenName not correct"                     idCorrect

        where
            txInfo :: TxInfo
            txInfo = scriptContextTxInfo ctx
            
            threadValue :: Value
            threadValue = singleton (threadToken info) (TokenName "thread") 1

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
            returnsThread = case (returnedThreadDatum, consumedThreadDatum) of
              (Just rd, Just td) -> mintCount rd == mintCount td + 1
              _       -> False

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
                [(_, tn, _)] -> unTokenName tn == appendByteString (tokenPrefix info) (intToBuiltinByteString threadedId) && 
                                threadedId < maxSupply info &&
                                threadedId >= 0
                _            -> False

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
    strings = map intToChar ints

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
  | otherwise = "0" -- fix this