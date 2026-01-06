{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TokenSwap where

import Plutus.V2.Ledger.Api
    ( ScriptContext (scriptContextTxInfo)
    , Validator
    , PubKeyHash
    , mkValidatorScript
    , adaSymbol
    , adaToken
    , singleton
    , TxInfo (txInfoInputs)
    , TxInInfo (txInInfoResolved)
    , TxOut (txOutAddress)
    )

import Plutus.V2.Ledger.Contexts
    ( valuePaidTo
    , ownHash
    )

import PlutusTx
    ( compile
    , unstableMakeIsData
    )

import PlutusTx.Prelude
    ( Bool (..)
    , Integer
    , (>=)
    , (==)
    , traceIfFalse
    , length
    , filter
    )

import Plutus.V1.Ledger.Address (scriptHashAddress)
import Utilities (wrapValidator, writeValidatorToFile)

-------------------------------------------------
-- Datum
-------------------------------------------------

data DatumSwap = DatumSwap
    { seller :: PubKeyHash
    , price  :: Integer
    }

PlutusTx.unstableMakeIsData ''DatumSwap

-------------------------------------------------
-- Validator
-------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx =
       traceIfFalse "Seller not paid enough ADA" sellerPaid
    && traceIfFalse "Only one script input allowed" singleScriptInput
  where
    info = scriptContextTxInfo ctx

    -- ✅ Accepts payment >= price
    sellerPaid :: Bool
    sellerPaid =
        valuePaidTo info (seller ds)
            `geq` singleton adaSymbol adaToken (price ds)

    -- ✅ Ensure exactly one script UTXO is consumed
    singleScriptInput :: Bool
    singleScriptInput =
        length (filter isOwnInput (txInfoInputs info)) == 1

    isOwnInput :: TxInInfo -> Bool
    isOwnInput i =
        txOutAddress (txInInfoResolved i)
            == scriptHashAddress (ownHash ctx)

-------------------------------------------------
-- Boilerplate
-------------------------------------------------

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: Validator
validator =
    mkValidatorScript $$(compile [|| mkWrappedValidator ||])

-------------------------------------------------
-- Helper
-------------------------------------------------

writeValidator :: IO ()
writeValidator =
    writeValidatorToFile "./assets/tokenswap.plutus" validator
