{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where
import Plutus.V2.Ledger.Api  (CurrencySymbol (CurrencySymbol), singleton, Value, TokenName (TokenName), MintingPolicy, 
                             BuiltinByteString, PubKeyHash (PubKeyHash), TxOutRef, Script)
import Test.Tasty            (testGroup, defaultMain)
import Plutus.Model          (logError, Run, testNoErrors, defaultBabbage, adaValue, mintValue, Tx, 
                             TypedPolicy (TypedPolicy), toV2, scriptCurrencySymbol, newUser, payToKey, spendScript, 
                             utxoAt, scriptHash, mintingPolicyHash, payToScript, DatumMode (HashDatum))
import Control.Monad         (unless)
import qualified ZingNFT as  OnChain
import qualified Thread  as  OnChain_Thread
import Utilities             (policyHash)
import ZingNFT (ContractInfo(threadToken))

testInfo :: CurrencySymbol -> OnChain.ContractInfo
testInfo cs = OnChain.ContractInfo cs 10 "ZingBoi"

zingBoiPolicy :: OnChain.ContractInfo -> TypedPolicy ()
zingBoiPolicy info = TypedPolicy $ toV2 $ OnChain.compiledPolicy info

threadPolicy :: TxOutRef -> TypedPolicy ()
threadPolicy out = TypedPolicy $ toV2 $ OnChain_Thread.compiledPolicy out

-- zingBoiCurrencySymbol :: CurrencySymbol
-- zingBoiCurrencySymbol = scriptCurrencySymbol (zingBoiPolicy testInfo)

-- zingBoiValue :: Value
-- zingBoiValue = singleton zingBoiCurrencySymbol "ZingBoi1" 1

-- zingBoi :: TxOutRef -> TypedPolicy ()
-- zingBoi out = zingBoiPolicy (testInfo (scriptCurrencySymbol (TypedPolicy $ toV2 (threadPolicy out))))


main :: IO ()
main = do
    defaultMain $ do
        testGroup 
            "test"
            [
                expectFail "temp" testMint
            ]
            where
                expectFail msg = expectSucceed msg 
                expectSucceed = testNoErrors (adaValue 10_000_000) defaultBabbage

-- mintTx :: Value -> PubKeyHash -> Tx
-- mintTx val u = 
--     mconcat
--     [
--         spendScript (zingBoiPolicy testInfo)
--         mintValue (zingBoiPolicy testInfo) () val,
--         payToKey u val
--     ]


mintThreadToken :: TypedPolicy () -> OnChain.ThreadDatum -> Value -> Script -> Tx
mintThreadToken pol dtm val spt = 
    mconcat
    [
        mintValue pol () val,
        payToScript spt (HashDatum dtm) val
    ]

testMint :: Run ()
testMint = do
    u1 <- newUser (adaValue 10)
    [(threadUtxoInput,_)] <- utxoAt u1

    let threadTokens = threadPolicy threadUtxoInput
    let zingBois = zingBoiPolicy (testInfo $ scriptCurrencySymbol threadTokens)

    let val = singleton (scriptCurrencySymbol threadTokens) "thread" 1

    let thread = mintThreadToken (threadPolicy threadUtxoInput) val (mintingPolicyHash zingBois)

    


    unless (False) $ logError "test"