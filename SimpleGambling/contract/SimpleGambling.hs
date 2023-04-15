module SimpleGambling where

import           Prelude              (IO, putStrLn)
import           Plutus.V2.Ledger.Api (ScriptContext)

-- simple raffle
-- one user lists an NFT
-- other users enter a raffle for that NFT, they pay a sum to the seller of entryPrice*entries to get x entries
-- the winner is selected randomly(not sure how yet), they can claim the token (dapp can build the tx for them)

-- script created with param:
-- txOutRef. 

-- single utxo with a bunch of 'entry' tokens in it at the start of the raffle
-- single utxo with the actual nft being raffled


-- to start:
-- supply tx:
    -- inputs: none
    -- outputs: 
        -- nft utxo, datum: {seller: x, entires: 500, price: 5} ) or param
        -- entries utxo, 500 entry tokens


-- Current state:
    -- scriptaddr:
    -- 1 nft utxo (datum: seller, entires, price)
    -- 1 utxo, 500 entry tokens

-- to enter
-- supply tx:
    -- inputs: entries utxo
    -- outputs:
        -- entries utxo at your address, with amount
        -- entries utxo at script address, with remaining entires
            -- to calculate that the entries are conserved, need to store the amount of entires on the NFT utxo as a datum
            -- nft utxo must be supplied as ref
        -- utxo at sellers address for ada = price * entires consumed
-- script checks:
-- 1. paid seller, price * entires consumed
-- 2. entries conserved (entries in users utxo + entires returned to script == (nft utxo datum).entires)


-- to claim:
--  




-- attach inline datum to NFT utxo?

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


