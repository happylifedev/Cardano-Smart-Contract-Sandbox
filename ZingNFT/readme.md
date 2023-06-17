## ZingNFT
A threaded and parameterised minting policy to mint custom NFTs

#### Revised approach notes:  
After revisiting my approach, I'm not sure it is possible to achieve this way. I realized that I was assuming I could send a token to a Minting policy and have it act like a spending validator. This is not the case, and instead the thread token would just sit at that address(some address?) but there would not be anything governing it's spending. 

I will need to create a spending validator that can own and govern the spending of the thread token. The current minting policy will need to adapt to cater for this change. 

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