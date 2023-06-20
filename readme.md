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

### ZingNFTTimeLocked
A time locking parameterized minting policy to allow minting of custom NFT's

### ZingNFT
WIP - Minting policy that is not timelocked but instead governed by a thread token  
#### High level approach:  
3 scripts required:  
1. thread policy
    - parameterized by a utxo, immutable
    - mints thread token
2. thread validator
    - governs spending of the thread token
    - verifies correct consumption and returning of the thread token
3. nft policy
    - policy to mint the actual nft
    - verifies params are correct

<img src="https://lh3.googleusercontent.com/fife/APg5EObOepa5HKORiiTfKKD8Jl0QlI5a0VOnWwNcYMw2O_5tlpAnbvUsj1lFcUZqLRtBX49zimh4ppoE30Ky9mj-iaIaB3ZJfN3yGCchOHC_E-MuvPqQLjWF9tos-CJBewc6D7yJp6styB8YKrI6HpdtH3Hzlw9b1FrpUTDXsABpFr5nFXQDtD8lAg1Dg9XG3uNaCTCxJM8s2BeHXwB_xcunf3ziFzsq06OdYO6C04GuCKuKwfug0jA6UqKLhUhM-EFa-u5BtbJ-FbydQ4JlWc9W3tbEZ25jqI7KYknvTKDNhPrMhQM_J47YOBoDDitUY39crl1HpyRHbekuVdu9jTWAbEv7GPXf0_ok55RBgsFh6xZC20QPB70uZ5NxMXK4qOXB0mknfH_Q5mMyfsZMNjuMYiU7nVhOqymwBYPtSbLYEEXHoojvswmM6khRu5230c08S-oDqVGDcwlqgfFmLXViyUCx7-VIWskF1VTVaOqrkRPgKo6idCfkzrZQSCPO3APBPLFpvTXZ5gW0l01hIzOwmQpffBMRd-X66_qdDHHAydLEOTqJSMy63cQ2gGpiO_KAWkkhoNNYAJlEuf-CH_f12O7H6M12KSzULZZ3oK0yCJkj00JhbvMzgRiUPEMFCd4u7WmvlMHeAWnx-peCkc2Bza0IBL4FJ00rEoiyp5uPU17yPgyUh9MN_mr4JjCotXMVa1SJhd8rClf8jOcuxc5CqMk8OHJaSl8x8oH5HcP1Y5v41IGyabPTTamWKo-55rcCyqq4fq7XNOOTcqdY3o_FIQZxrhyelSPXwq6ZQksGnP8oyIhX53qOipENadTvhKGPVeuTat3IYEUugryuF-JBLifso3J6RMWt_yyr38PLaAxpByc7QeB6ABmqg2zA4ciupojWYbDJjgn4PIgD40_vbW4_w-mVr-96i4jAZlSYAPGD7uaLxlCooHlCF34gqNJiSHTWKcDwLVqGxom_gPSJ84O2Sn6QwyMkoIxEdHXZ__FafNPRTdv20v9JMG7AfpBT4mVgbcasfbgIr4qP8UUnHQ=w1920-h1080" width="500"/>


### SimpleRaffle
wip - List an NFT for raffle, users buy entries to win the token, a winner is drawn and the NFT is claimed by the winner. 


#### Plutus docs
cd /app/plutus-pioneer-program 
python3 -m http.server -d docs/plutus-docs/haddock/