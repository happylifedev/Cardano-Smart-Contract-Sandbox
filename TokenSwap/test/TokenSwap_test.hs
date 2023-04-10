{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad        (mapM, unless)
import           Plutus.Model         (Run,
                                       TypedValidator (TypedValidator),
                                       adaValue, defaultBabbage, mustFail, testNoErrors,
                                       toV2, FakeCoin (FakeCoin), fakeValue, newUser,
                                       UserSpend, userSpend, payToScript,
                                       Tx, DatumMode (HashDatum), payToKey, spendScript,
                                       spend, submitTx, utxoAt, logError, valueAt)
import           Plutus.V2.Ledger.Api (PubKeyHash, Value, TxOutRef)
import           PlutusTx.Prelude     (($))
import           Prelude              (IO, (.), (<>), Monoid (mconcat),
                                       (==), (&&), (!!), head)
import qualified TokenSwap            as OnChain
import           Test.Tasty           (defaultMain, testGroup)


main :: IO ()
main = do
  defaultMain $ do
    testGroup
      "Catch double spend with testing"
      [ expectSuceed "Normal spending" normalSpending
      , bad  "Double spending" doubleSpending
      ]
 where
    bad msg = expectSuceed msg . mustFail
    expectSuceed = testNoErrors (adaValue 10_000_000 <> fakeValue scToken 100) defaultBabbage


scToken :: FakeCoin
scToken = FakeCoin "Super-Cool-Token"
 
type HomeworkScript = TypedValidator OnChain.DatumSwap ()

swapScript :: HomeworkScript
swapScript = TypedValidator $ toV2 OnChain.validator

lockingTx :: UserSpend -> Value -> OnChain.DatumSwap -> Tx
lockingTx usp val dtm = 
    mconcat 
    [
        userSpend usp
      , payToScript swapScript (HashDatum dtm) val
    ]

consumingTx :: UserSpend -> PubKeyHash -> PubKeyHash -> Value -> TxOutRef -> OnChain.DatumSwap -> Tx
consumingTx usp buyer seller tokenVal output dtm = 
    mconcat 
    [
        userSpend usp,
        spendScript swapScript output () dtm,
        payToKey buyer tokenVal,
        payToKey seller $ adaValue 100
    ]


doubleConsumingTx :: UserSpend -> PubKeyHash -> PubKeyHash -> TxOutRef -> TxOutRef -> OnChain.DatumSwap -> Tx
doubleConsumingTx usp buyer seller ref1 ref2 dtm =
    mconcat [
        userSpend usp,
        spendScript swapScript ref1 () dtm,
        spendScript swapScript ref2 () dtm,
        -- 2 utxos
        payToKey buyer $ fakeValue scToken 1,
        payToKey buyer $ fakeValue scToken 1,
        -- 1 utxo
        -- payToKey buyer $ fakeValue scToken 2,
        payToKey seller $ adaValue 100
    ]


normalSpending :: Run ()
normalSpending = do
    let tokenVal = fakeValue scToken 1
    u1 <- newUser tokenVal             -- user1 has token
    u2 <- newUser $ adaValue 100       -- user2 has 100 ada
    let dtm = OnChain.DatumSwap u1 100 -- beneficiary(of the ada) user u1, price 100 ada

    -- lock utxo
    lockingSpend <- spend u1 tokenVal
    submitTx u1 $ lockingTx lockingSpend tokenVal dtm

    utxos <- utxoAt swapScript
    let (ref, _) = head utxos

    consumingSpend <- spend u2 (adaValue 100)
    submitTx u2 $ consumingTx consumingSpend u2 u1 tokenVal ref dtm
    [v1, v2] <- mapM valueAt [u1, u2]
    unless (v1 == adaValue 100 && v2 == tokenVal) 
        $ logError "Final balances aren't correct"

doubleSpending :: Run ()
doubleSpending = do
    u1 <- newUser $ fakeValue scToken 2 -- user1 has token
    u2 <- newUser $ adaValue 100        -- user2 has 100 ada
    let dtm = OnChain.DatumSwap u1 100  -- beneficiary(of the ada) user u1, price 100 ada

    -- lock 2 utxos
    lockingSpend1 <- spend u1 $ fakeValue scToken 1
    submitTx u1 $ lockingTx lockingSpend1 (fakeValue scToken 1) dtm

    lockingSpend2 <- spend u1 $ fakeValue scToken 1
    submitTx u1 $ lockingTx lockingSpend2 (fakeValue scToken 1) dtm
    
    utxos <- utxoAt swapScript
    let (ref1, _) = head utxos
    let (ref2, _) = utxos !! 1

    consumingSpend <- spend u2 (adaValue 100)
    submitTx u2 $ doubleConsumingTx consumingSpend u2 u1 ref1 ref2 dtm

    [v1, v2] <- mapM valueAt [u1, u2]
    unless (v1 == adaValue 100 && v2 == fakeValue scToken 2)
        $ logError "Final balances aren't correct"