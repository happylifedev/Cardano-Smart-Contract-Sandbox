import { 
    Lucid, 
    Blockfrost,
    SpendingValidator,
    Data,
    Unit,
    TxHash
} from "https://deno.land/x/lucid@0.9.8/mod.ts";

// need keyfile.json file in ./
import { blockfrostKey, key1, key2 } from "./keyfile.json" assert { type: "json" }

const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      blockfrostKey
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

async function lockToken(key: string): Promise<TxHash> {
    lucid.selectWalletFromSeed(key);

    const dtm = {
        seller: lucid.utils.getAddressDetails(await lucid.wallet.address()).address.hex,
        price: BigInt(10)
    }

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

async function buyToken(key: string): Promise<TxHash> {
    // TODO send utxo to seller, claim token
}

async function retrieveToken(key: string): Promise<TxHash> {
    // TODO seller retrieve unsold token
}

function run () {
    console.log('running')
}

run();