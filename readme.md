# plutus-contracts

This repo serves as a sandbox for smart contract experiments on the cardano blockchain. Each file represents an idea for a self contained contract, along with unit tests and usage examples.

### TokenSwap
Allow a user to list an NFT for sale, that can be bought by other users as long as they pay the seller the required price. The seller can cancel their sale if the NFT has not been sold. 

### Vesting
wip - Lock a utxo at a script address that only a specified beneficairy can unlock, after a given deadline

### SimpleGambling
wip - pay a fee to have a chance at winning a prize(might become raffle)

### ZingCoin
wip - minting policy to allow minting of custom native tokens

### ZingNFT
wip - minting policy to allow minting of custom NFT's


#### Plutus docs
cd /app/plutus-pioneer-program 
python3 -m http.server -d docs/plutus-docs/haddock/