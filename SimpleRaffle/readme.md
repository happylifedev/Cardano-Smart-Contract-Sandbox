## Simple Raffle

### Use case
1. a user lists an NFT for raffling
2. other users enter a raffle for that NFT by buying entries, 
    - they pay a sum to the seller of entryPrice*entries to get x entries
3. the winner is selected randomly(not sure how yet), they can claim the token (dapp can build the tx for them)


### Approach 

- need to mint entry tokens
    - minting policy, centralized, only single pubkeyhash allowed to mint
    - endless supply, no value
    - must be tied to each raffle
        - otherwise could buy cheap entires and redeem other raffles

#### State required

Per raffle:  
`deadline`: when the raffle ends  
`tickets`: amount of tickets available  
`ticketPrice`: cost of one ticket  
`seller`: the original owner of the token  

Per entry:  
`entries`: the amount of entries bought  
`entriesLeft`: the left over entries after purchase

Per claim:  
`winner`: who won the raffle

#### Outline

1. To start:
    - Send to script address:
        - `inputs`: 
        - `outputs`:   
            - `nft utxo`  
            - `entries utxo` 500 entry tokens

1. (a) To cancel:?

2. to enter
    - Supply Tx:
        - `inputs`: 
            1. `entries utxo` 500(current available) entry tokens
        - `outputs`:
            1. `entries utxo` 20(entries) at your address
            2. `entries utxo` 480(entries change) at script address
                - to calculate that the entries are conserved, need to store the amount of entires on the `NFT utxo` as a datum(?)
                - `nft utxo` must be supplied as ref
            3. `seller utxo` utxo at sellers address for price * entires consumed
    - script checks Tx:
        - 1. paid seller, price * entires consumed
        - 2. entries conserved (entries in users utxo + entires returned to script == total entries in nft utxo datum)
        - 3. deadline not exceeded 
        - 4. entries are sent to buyer

3. to claim:
    - Supply Tx:
        - `inputs` 
            1. `entries utxo` return entries
            2. 
        - `outputs` 
            1. `nft utxo` the nft that was won

    - script checks:
        - deadline passed
        - claimer supplied entries
        - claimer is winner ??
            - at point of claim, we will know:
                - buyers entry count
                - buyer 
                - 
            - rules:
                1. there must be exactly 1 winner in all cases
                2. can't be precoded into the script
                    - because the script will need to be supplied on the frontend(accessible)
