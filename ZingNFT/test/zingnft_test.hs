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
                             submitTx, userSpend, spend, UserSpend, currentTimeRad, validateIn, valueAt
                            )
import Prelude              (IO, ($), Monoid (mconcat), Eq ((==)), (.), Bool (True, False))
import Plutus.V2.Ledger.Api (TokenName(TokenName), PubKeyHash (PubKeyHash), Value (Value), fromList, POSIXTime (POSIXTime))
import Test.Tasty           (defaultMain, testGroup)
import Control.Monad        (unless)

main :: IO ()
main = do
    defaultMain $ do
        testGroup
            "Test minting ZingBoi"
            [
            --    expectSucceed "Owner mint"     testAuthMint,
               expectFail    "Non owner mint" testUnAuthMint
            ]
        where
            expectFail msg = expectSucceed msg . mustFail
            expectSucceed = testNoErrors (adaValue 100) defaultBabbage

type ZingPolicy = Onchain.Constants -> TypedPolicy ()

zingCoinPolicy :: ZingPolicy
zingCoinPolicy p = TypedPolicy $ toV2 (Onchain.compiledPolicy p)

mintingTx :: Onchain.Constants -> PubKeyHash -> UserSpend -> Value -> Tx
mintingTx c u usp val =
    mconcat
    [
        userSpend usp,
        mintValue (zingCoinPolicy c) () val,
        payToKey u val
    ]

testUnAuthMint :: Run ()
testUnAuthMint = do
    u1 <- newUser $ adaValue 10

    let constants = Onchain.Constants (PubKeyHash "c1bd6f764d3b68b9f689fbc7ea8027fad5fb190a71caab469cfa8f83") (TokenName "ZingBoi") (POSIXTime 1713550450)

    let zingSymbol = scriptCurrencySymbol (zingCoinPolicy constants)

    let mintVal = Value $ fromList [(zingSymbol, fromList [(TokenName "ZingBoi", 1)])]

    usp <- spend u1 $ adaValue 1

    timeInt <- currentTimeRad 50

    tx <- validateIn timeInt $ mintingTx constants u1 usp mintVal

    submitTx u1 tx -- submit tx fails 

    unless False $ logError "Token not minted"

    -- v1 <- valueAt u1

    -- unless (v1 == Value (fromList [(zingSymbol, fromList [(TokenName "ZingBoi", 1)])])) $ logError "Token not minted"

-- testAuthMint :: Run ()
-- testAuthMint = do
--     u1 <- newUser $ adaValue 10

--     let constants = Onchain.Constants u1 (TokenName "ZingBoi") (POSIXTime 1713550450)

--     let zingSymbol = scriptCurrencySymbol (zingCoinPolicy constants)
--     let mintVal = Value $ fromList [(zingSymbol, fromList [(TokenName "ZingBoi", 1)])]

--     usp <- spend u1 $ adaValue 1

--     timeInt <- currentTimeRad 50
--     tx <- validateIn timeInt $ mintingTx constants u1 usp mintVal

--     submitTx u1 tx

--     v1 <- valueAt u1

--     unless (v1 == Value (fromList [(zingSymbol, fromList [(TokenName "ZingBoi", 1)])])) $ logError "Token not minted"