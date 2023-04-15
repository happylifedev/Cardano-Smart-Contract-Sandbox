{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Prelude              (IO, ($), (.), Monoid (mconcat), head, Traversable (mapM), (==), (&&), Bool)
import Test.Tasty           (defaultMain, testGroup)
import Plutus.Model         (
                            testNoErrors, adaValue, defaultBabbage, mustFail, TypedValidator (TypedValidator),
                            Run, newUser, spend, Tx, UserSpend, userSpend, payToScript, toV2, DatumMode (HashDatum),
                            submitTx, utxoAt, spendScript, payToKey, valueAt, logError, waitUntil, validateIn, currentTimeRad, noErrors
                            )
import Plutus.V1.Ledger.Api (PubKeyHash, Value, TxOutRef, POSIXTime (POSIXTime))
import qualified Vesting as Onchain
import Control.Monad


main :: IO ()
main = do 
    defaultMain $ do 
        testGroup 
            "Test Vesting"
            [ expectSucceed "Normal vesting" normalVesting
            , expectFail "Claim before deadline" earlyClaim
            , expectFail "Try to steal" incorrectBeneficiary
            ]
    where
        expectFail msg = expectSucceed msg . mustFail
        expectSucceed = testNoErrors (adaValue 10_000_00) defaultBabbage

type VestingScript = TypedValidator Onchain.VestingDatum ()

vestingScript :: VestingScript
vestingScript = TypedValidator $ toV2 Onchain.compiledValidator

lockingTx :: UserSpend -> Onchain.VestingDatum -> Value -> Tx
lockingTx usp dtm val = 
    mconcat [
        userSpend usp,
        payToScript vestingScript (HashDatum dtm) val
    ]

consumeTx :: TxOutRef -> PubKeyHash -> Onchain.VestingDatum -> Value -> Tx
consumeTx ref pkh dtm val =
    mconcat [
        spendScript vestingScript ref () dtm,
        payToKey pkh val
    ]

normalVesting :: Run ()
normalVesting = do
    u1 <- newUser $ adaValue 100
    u2 <- newUser $ adaValue 100

    let lockValue = adaValue 10

    let dtm = Onchain.VestingDatum u2 $ POSIXTime 1000

    u1Sp <- spend u1 lockValue
    submitTx u1 $ lockingTx u1Sp dtm lockValue

    waitUntil $ POSIXTime 1100

    utxos <- utxoAt vestingScript
    let (ref, _) = head utxos

    timeInt <- currentTimeRad 50
    tx <- validateIn timeInt $ consumeTx ref u2 dtm lockValue

    submitTx u2 tx

    [v1, v2] <- mapM valueAt [u1, u2]
    unless (v1 == adaValue 90 && v2 == adaValue 110) 
        $ logError "Final balances aren't correct"

earlyClaim :: Run Bool
earlyClaim = do 
    u1 <- newUser $ adaValue 100
    u2 <- newUser $ adaValue 100

    let lockValue = adaValue 10

    let dtm = Onchain.VestingDatum u2 $ POSIXTime 1000

    u1Sp <- spend u1 lockValue
    submitTx u1 $ lockingTx u1Sp dtm lockValue

    waitUntil $ POSIXTime 500

    utxos <- utxoAt vestingScript
    let (ref, _) = head utxos

    timeInt <- currentTimeRad 50
    tx <- validateIn timeInt $ consumeTx ref u2 dtm lockValue

    submitTx u2 tx

    noErrors

incorrectBeneficiary :: Run Bool
incorrectBeneficiary = do
    u1 <- newUser $ adaValue 100
    u2 <- newUser $ adaValue 100
    u3 <- newUser $ adaValue 100

    let lockValue = adaValue 10

    let dtm = Onchain.VestingDatum u3 $ POSIXTime 1000

    u1Sp <- spend u1 lockValue
    submitTx u1 $ lockingTx u1Sp dtm lockValue

    waitUntil $ POSIXTime 1100

    utxos <- utxoAt vestingScript
    let (ref, _) = head utxos

    timeInt <- currentTimeRad 50
    tx <- validateIn timeInt $ consumeTx ref u2 dtm lockValue

    submitTx u2 tx

    noErrors