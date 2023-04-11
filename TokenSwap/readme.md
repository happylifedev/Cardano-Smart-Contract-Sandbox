# TokenSwap

Contract to facilitate the sale of NFT's via smart contract.

### Summary

One user(the seller) locks an NFT at the script address, and includes an inline datum with details in the form:
```
{
    seller: PubKeyHash,
    price: 100
}
```
A buyer must create a transaction that includes a UTXO at the sellers address, for the specified price, as well as the UTXO at their own address containing the NFT for the transaction to succeed.

The seller also has the option to cancel the listing and retrieve the NFT if it has not yet been sold.


