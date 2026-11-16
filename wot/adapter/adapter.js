// Adapter template: converts the payload of a non-WoT source (e.g. a decoded LoRaWAN
// uplink or a third-party weather-station API) into the units/names of the Thing
// Description and writes it to the Thing's Properties (HTTP POST) with node-wot.
//
// Usage:
//   node adapter/adapter.js --td <TD url or file> --mapping <mapping.json> --payload <payload.json>
// Optional: set WOT_TOKEN to send a bearer token (needed on the real platform).
"use strict";
const fs = require("fs");
const { Servient } = require("@node-wot/core");
const { HttpClientFactory } = require("@node-wot/binding-http");

function arg(name) {
  const i = process.argv.indexOf("--" + name);
  return i > -1 ? process.argv[i + 1] : undefined;
}
const get = (obj, path) => path.split(".").reduce((o, k) => (o == null ? undefined : o[k]), obj);

// payload -> { propertyName: { timestamp, value } } in the units of the TD
function normalise(payload, mapping) {
  const timestamp = new Date(get(payload, mapping.timestamp)).toISOString();
  const out = {};
  for (const [prop, m] of Object.entries(mapping.properties)) {
    const raw = get(payload, m.source);
    if (typeof raw !== "number" || Number.isNaN(raw)) continue;   // missing value: skip, do not invent
    out[prop] = { timestamp, value: Number((raw * (m.scale ?? 1) + (m.offset ?? 0)).toFixed(6)) };
  }
  return out;
}

async function main() {
  const [tdSrc, mappingFile, payloadFile] = [arg("td"), arg("mapping"), arg("payload")];
  if (!tdSrc || !mappingFile || !payloadFile) {
    console.error("Usage: node adapter/adapter.js --td <url|file> --mapping <file> --payload <file>");
    process.exit(1);
  }
  const mapping = JSON.parse(fs.readFileSync(mappingFile, "utf8"));
  const payload = JSON.parse(fs.readFileSync(payloadFile, "utf8"));
  const values = normalise(payload, mapping);

  const servient = new Servient();
  servient.addClientFactory(new HttpClientFactory());
  const WoT = await servient.start();

  const td = /^https?:\/\//.test(tdSrc)
    ? await WoT.requestThingDescription(tdSrc)
    : JSON.parse(fs.readFileSync(tdSrc, "utf8"));
  if (process.env.WOT_TOKEN && td.id) {
    servient.addCredentials({ [td.id]: { token: process.env.WOT_TOKEN } });
  }
  const thing = await WoT.consume(td);

  for (const [prop, v] of Object.entries(values)) {
    await thing.writeProperty(prop, v);
    console.log("written", prop, JSON.stringify(v));
  }
  await servient.shutdown();
}
main().catch((e) => { console.error(e); process.exit(1); });

module.exports = { normalise };
