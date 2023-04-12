import { 
    Lucid, 
    Blockfrost,
    SpendingValidator,
    Data,
    Unit,
    TxHash
} from "https://deno.land/x/lucid@0.9.8/mod.ts";

// need keyfile.json file in ./
import keys from "./keyfile.json" assert { type: "json" }

const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      keys.blockfrostKey
    ),
    "Preprod"
);

// validator
const TokenSwapValidator : SpendingValidator = {
    type: "PlutusV2",
    script: "" //compile script and add here
}

// const TokenUnit: Unit = "ZingCoinMintingPolicyHex" + fromText("ZingCoin")
const TokenUnit: Unit = "" // note: need to actually make a token here

// datum shape
const SwapDatum = Data.Object({
    seller: Data.Bytes(),
    price: Data.Integer()
})
type SwapDatum = Data.Static<typeof SwapDatum>;

async function lockToken(key: string, dtm: SwapDatum): Promise<TxHash> {
    lucid.selectWalletFromSeed(key);

    const tx = await lucid
        .newTx()
        .payToContract(
            lucid.utils.validatorToAddress(TokenSwapValidator),
            { inline: Data.to<SwapDatum>(dtm, SwapDatum) },
            { TokenUnit: BigInt(1) })
        .complete()

    const signed = await tx.signWithPrivateKey(key).complete();
    const hash = await signed.submit();
    return hash;
}

async function buyToken(key: string, dtm: SwapDatum): Promise<TxHash> {
    lucid.selectWalletFromPrivateKey(key);

    const scriptAddr = lucid.utils.validatorToAddress(TokenSwapValidator);

    const utxos = await lucid.utxosAt(scriptAddr);

    const tokenUtxo = utxos.find(u => u.datum == Data.to<SwapDatum>(dtm, SwapDatum));

    if(tokenUtxo !== undefined) {

        const tx = await lucid
                    .newTx()
                    .collectFrom([tokenUtxo], Data.void())
                    .attachSpendingValidator(TokenSwapValidator)
                    .complete();

        const signedTx = await tx.sign().complete();
        const hash = signedTx.submit();

        return hash;

    } else {
        throw new Error("No Utxo's at script!")
    }
}

async function retrieveToken(key: string, dtm: SwapDatum): Promise<TxHash> {
    lucid.selectWalletFromPrivateKey(key);

    const scriptAddr = lucid.utils.validatorToAddress(TokenSwapValidator);

    const utxos = await lucid.utxosAt(scriptAddr);

    const tokenUtxo = utxos.filter(u => u.datum == Data.to<SwapDatum>(dtm, SwapDatum));

    if (tokenUtxo !== undefined) {
        const Tx = await lucid
                    .newTx()
                    .collectFrom(tokenUtxo, Data.void())
                    .attachSpendingValidator(TokenSwapValidator)
                    .complete();

        const signedTx = await Tx.sign().complete();
        const hash = signedTx.submit();

        return hash;
    } else {
        throw new Error("No utxo found at script!")
    }
}

async function run () {

    const dtm = {
        seller: lucid.utils.getAddressDetails(await lucid.wallet.address()).address.hex,
        price: BigInt(10)
    }

    await lockToken(keys.key1, dtm);
    
    await buyToken(keys.key2, dtm);

    // await retrieveToken(keys.key1, dtm);

}

run();
