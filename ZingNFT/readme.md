## ZingNFT
A threaded and parameterised minting policy to mint custom NFTs

### Revised notes:  
After revisiting my approach, I'm not sure it is possible to achieve my goal in the way I thought previously. I realized that I was assuming I could send a token to a Minting policy and have it act like a spending validator. This is not the case, and instead the thread token would just sit at that address(some address?) but there would not be anything governing it's spending. 

I will need to create a spending validator that can own and govern the spending of the thread token. The current minting policy will need to adapt to cater for this change. 

### 2023-06-18 Approach

#### 3 scripts

1. Thread minting policy
    - mints the thread token
    - parameterised by a utxo  
        - garauntees non-fungibility
2. Thread spending validator
    - the address that the thread token will live at
    - validates that the thread token is spent and returned correctly
    - linked to thread token, and NFT contract
        - parameterised by the thread policy symbol
        - parameterised by the NFT policy symbol
    - verifies:
        - thread token is consumed and returned
        - datum is valid: mintCount incrememnted correctly, other params retained
        - tx mints an NFT
3. NFT minting policy
    - governs minting of the zingboi
    - linked to the thread token
        - parameterised by the thread symbol
    - verifies that a thread token is consumed

![StateMachine](https://lh3.googleusercontent.com/fife/APg5EObOepa5HKORiiTfKKD8Jl0QlI5a0VOnWwNcYMw2O_5tlpAnbvUsj1lFcUZqLRtBX49zimh4ppoE30Ky9mj-iaIaB3ZJfN3yGCchOHC_E-MuvPqQLjWF9tos-CJBewc6D7yJp6styB8YKrI6HpdtH3Hzlw9b1FrpUTDXsABpFr5nFXQDtD8lAg1Dg9XG3uNaCTCxJM8s2BeHXwB_xcunf3ziFzsq06OdYO6C04GuCKuKwfug0jA6UqKLhUhM-EFa-u5BtbJ-FbydQ4JlWc9W3tbEZ25jqI7KYknvTKDNhPrMhQM_J47YOBoDDitUY39crl1HpyRHbekuVdu9jTWAbEv7GPXf0_ok55RBgsFh6xZC20QPB70uZ5NxMXK4qOXB0mknfH_Q5mMyfsZMNjuMYiU7nVhOqymwBYPtSbLYEEXHoojvswmM6khRu5230c08S-oDqVGDcwlqgfFmLXViyUCx7-VIWskF1VTVaOqrkRPgKo6idCfkzrZQSCPO3APBPLFpvTXZ5gW0l01hIzOwmQpffBMRd-X66_qdDHHAydLEOTqJSMy63cQ2gGpiO_KAWkkhoNNYAJlEuf-CH_f12O7H6M12KSzULZZ3oK0yCJkj00JhbvMzgRiUPEMFCd4u7WmvlMHeAWnx-peCkc2Bza0IBL4FJ00rEoiyp5uPU17yPgyUh9MN_mr4JjCotXMVa1SJhd8rClf8jOcuxc5CqMk8OHJaSl8x8oH5HcP1Y5v41IGyabPTTamWKo-55rcCyqq4fq7XNOOTcqdY3o_FIQZxrhyelSPXwq6ZQksGnP8oyIhX53qOipENadTvhKGPVeuTat3IYEUugryuF-JBLifso3J6RMWt_yyr38PLaAxpByc7QeB6ABmqg2zA4ciupojWYbDJjgn4PIgD40_vbW4_w-mVr-96i4jAZlSYAPGD7uaLxlCooHlCF34gqNJiSHTWKcDwLVqGxom_gPSJ84O2Sn6QwyMkoIxEdHXZ__FafNPRTdv20v9JMG7AfpBT4mVgbcasfbgIr4qP8UUnHQ=w1920-h1080)

---

### 2023-04-27 approach:

- Mint x thread tokens  
    - inline datum
        - currentMinted = 0  
        - index of x(eg. 4 thread tokens, each must have 0,1,2,3 in their datum)  
    - mint is based on single utxo
- Pay those thread tokens to the actual minting policy at time of minting them  
- Minting policy is parameterized by the thread token assetClass  
- Policy checks
    1. each mint consumes and creates a thread token with n+1 currentMinted  
    2. currentMinted <= threadtoken.datum.totalSupply/x (eg. 123 <= 1000/4)  
    3. actual nft id == (threadtoken.datum.pos * totalSupply) + threadtoken.currentMinted  