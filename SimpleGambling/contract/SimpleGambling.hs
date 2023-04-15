--module SimpleGambling where


-- simple raffle
-- one user lists an NFT
-- other users enter a raffle for that NFT, they pay a sum to the seller of entryPrice*entries to get x entries
-- the winner is selected randomly(not sure how yet), they can claim the token (dapp can build the tx for them)

-- need to mint entry tokens



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
