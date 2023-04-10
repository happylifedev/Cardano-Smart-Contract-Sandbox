module SimpleGambling where

import           Prelude              (IO, putStrLn)
import           Plutus.V2.Ledger.Api (ScriptContext)

-- parameter sets unlock number
-- data GuessParam = GuessParam {
--     key :: Integer
-- }
-- mkLift ''GuessParam

-- -- store price in datum
-- data PriceDatum = PriceDatum {
--     price :: Integer,
--     host :: PubKeyHash
-- }
-- unstableMakeIsData ''PriceDatum


-- -- contract checks that output exists for price at some address
-- -- if redeemer contains the right number, utxo is allowed to be spent

-- mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-- mkValidator :: dtm r ctx = traceIfFalse "You didn't pay the host!" paidOwner &&
--                            traceIfFalse "Guess was not correct" redeemerMatches
--     where
--         txInfo :: txInfo
--         txInfo = scriptContextTxInfo ctx

--         redeemerMatches :: Bool
--         redeemerMatches = False

--         paidOwner :: Bool
--         paidOwner = False


