
import {
    Lucid
} from "https://deno.land/x/lucid@0.9.8/mod.ts";

import seeds from './keyfile.json' assert {type: "json"}

const addr = await (await Lucid.new(undefined,"Preprod")).selectWalletFromSeed(seeds.seed).wallet.address()

console.log(addr)