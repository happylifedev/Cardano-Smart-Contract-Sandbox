# plutus-contracts

This repo serves as a sandbox for smart contract experiments on the cardano blockchain. Each file represents an idea for a self contained contract, along with unit tests and usage examples.

### Vesting
Lock a utxo at a script address that only a specified beneficairy can unlock, after a given deadline

### TokenSwap
Allow a user to list an NFT for sale, that can be bought by other users as long as they pay the seller the required price. The seller can cancel their sale if the NFT has not been sold. 

### ZingCoin
Minting policy to allow (inifite, and centralized)minting of a native token called ZingCoin

### ZingCoinParameterized
Zingcoin except with the script constants applied to the compiled code, by Lucid.

### SimpleRaffle
wip - List an NFT for raffle, users buy entries to win the token, a winner is drawn and the NFT is claimed by the winner. 

### ZingNFT
wip - minting policy to allow minting of custom NFT's

#### Plutus docs
cd /app/plutus-pioneer-program 
python3 -m http.server -d docs/plutus-docs/haddock/