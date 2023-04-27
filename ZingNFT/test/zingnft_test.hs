{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}


module Main where

import qualified ZingNFT as Onchain
import Plutus.Model         (
                             Tx, Run, logError, newUser, adaValue, testNoErrors, defaultBabbage, mustFail,
                             TypedPolicy (TypedPolicy), toV2, mintValue, payToKey, scriptCurrencySymbol,
                             submitTx, userSpend, spend, UserSpend, currentTimeRad, validateIn, valueAt, noErrors, waitUntil
                            )
import Prelude              (IO, ($), Monoid (mconcat), Eq ((==)), (.), Bool (False), Semigroup ((<>)), putStrLn)
import Plutus.V2.Ledger.Api (TokenName(TokenName), PubKeyHash, Value (Value), fromList, POSIXTime (POSIXTime))
import Test.Tasty           (defaultMain, testGroup)
import Control.Monad        (unless)

main :: IO ()
main = do
    defaultMain $ do
        testGroup
            "Test minting ZingBoi"
            [
               expectSucceed "Owner mint"      testAuthMint,
               expectFail    "Non-owner mint"  testUnAuthMint,
               expectFail    "Mint after lock" afterLocked
            ]
        where
            expectFail msg = expectSucceed msg . mustFail
            expectSucceed = testNoErrors (adaValue 100) defaultBabbage

type ZingPolicy = Onchain.Constants -> TypedPolicy ()

zingCoinPolicy :: ZingPolicy
zingCoinPolicy p = TypedPolicy $ toV2 (Onchain.compiledPolicy p)

mintingTx :: Onchain.Constants -> PubKeyHash -> Value -> Tx
mintingTx c u val =
    mconcat
    [
        mintValue (zingCoinPolicy c) () val,
        payToKey u val
    ]

testAuthMint :: Run ()
testAuthMint = do
    u1 <- newUser $ adaValue 10

    let constants = Onchain.Constants u1 "ZingBoi" (POSIXTime 1714116796000)
    let zingSymbol = scriptCurrencySymbol (zingCoinPolicy constants)
    let mintVal = Value $ fromList [(zingSymbol, fromList [(TokenName "ZingBoi001", 1)])]

    timeInt <- currentTimeRad 50
    tx <- validateIn timeInt $ mintingTx constants u1 mintVal

    submitTx u1 tx

    v1 <- valueAt u1

    unless (v1 == mintVal <> adaValue 10) $ logError "Balance at address not correct"

testUnAuthMint :: Run Bool
testUnAuthMint = do
    u1 <- newUser $ adaValue 10

    let constants = Onchain.Constants "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83" "ZingBoi" (POSIXTime 1714116796000)

    let zingSymbol = scriptCurrencySymbol (zingCoinPolicy constants)

    let mintVal = Value $ fromList [(zingSymbol, fromList [(TokenName "ZingBoi001", 1)])]

    timeInt <- currentTimeRad 50

    tx <- validateIn timeInt $ mintingTx constants u1 mintVal

    submitTx u1 tx

    noErrors

afterLocked :: Run Bool
afterLocked = do
    u1 <- newUser $ adaValue 10

    let waitToConsume = POSIXTime 1682601577000
    
    let constants = Onchain.Constants u1 "ZingBoi" (POSIXTime 1682601557000)
    let zingSymbol = scriptCurrencySymbol (zingCoinPolicy constants)
    let mintVal = Value $ fromList [(zingSymbol, fromList [(TokenName "ZingBoi001", 1)])]

    timeInt <- currentTimeRad 50
    tx <- validateIn timeInt $ mintingTx constants u1 mintVal

    waitUntil waitToConsume

    submitTx u1 tx

    noErrors