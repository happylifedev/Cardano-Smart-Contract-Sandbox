import {
    Lucid, fromText,

} from "https://deno.land/x/lucid@0.9.8/mod.ts";

import seeds from './keyfile.json' assert {type: "json"}

const lucid =await Lucid.new(undefined,"Preprod");

const addr = await lucid.selectWalletFromSeed(seeds.seed).wallet.address()

console.log("bech32", addr)
console.log("hex", fromText(addr))