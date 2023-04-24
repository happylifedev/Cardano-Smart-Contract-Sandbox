## WIP


### Approach:

- Mint x thread tokens  
    - inline datum
        - currentMinted = 0  
        - position of x(eg. 4 thread tokens, each must have 0,1,2,3 in their datum)  
- Pay those thread tokens to the actual minting policy at time of minting them  
- Minting policy is parameterized by the thread token assetClass  
- Policy checks
    1. each mint consumes and creates a thread token with n+1 currentMinted  
    2. currentMinted <= threadtoken.datum.totalSupply/x (eg. 123 <= 1000/4)  
    3. actual nft id == (threadtoken.datum.pos * totalSupply) + threadtoken.currentMinted  
