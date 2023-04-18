module Main where

import Plutus.Model          (mustFail, adaValue, defaultBabbage, testNoErrors, TypedValidator, TypedPolicy (TypedPolicy), toV2, newUser, Tx, Run, UserSpend, mintValue, logError)
import Test.Tasty            (defaultMain, testGroup)
import Prelude               (IO, ($), (.), Bool (True), Monoid (mconcat), putStrLn)
import qualified ZingCoin as OnChain
import Plutus.V2.Ledger.Api (Value)
import Control.Monad

main :: IO ()
main = putStrLn "Not implemented"

-- main :: IO ()
-- main = do 
--     defaultMain $ do
--         testGroup
--             "Test ZingCoin"
--             [
--                 expectFail "Plebs can't mint" plebsCantMind  
-- --            , expectSucceed "Owner can mint" ownerCanMint
--             ]
--         where
--             expectFail msg = expectSucceed msg . mustFail
--             expectSucceed = testNoErrors (adaValue 100) defaultBabbage

-- type ZingCoinScript = TypedPolicy OnChain.CoinParams

-- coinScript :: ZingCoinScript
-- coinScript = TypedPolicy $ toV2 (OnChain.compiledPolicy OnChain.zingCoinParams)

-- consumingTx :: UserSpend -> Value -> Tx
-- consumingTx usp val = 
--     mconcat 
--     [
--         mintValue coinScript val ?
--     ]

-- -- ownerCanMint :: Run ()
-- -- ownerCantMint = do
-- --     not sure how to create a user with a pre-decided pubkeyhash


-- plebsCantMint :: Run ()
-- plebsCantMint = do
--     u1 <- newUser $ adaValue 10

--     unless (True) $ logError "ayo"








