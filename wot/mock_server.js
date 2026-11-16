// Local mock of a WoT endpoint, built with node-wot. It exposes the properties of a
// Thing Description (read: GET, write: POST) and keeps the written values in memory,
// so that the adapter can be tested without access to the ACQUAOUNT platform.
//
// Usage: node mock_server.js thing_descriptions/field_TEMPLATE.td.json [port]
"use strict";
const fs = require("fs");
const { Servient } = require("@node-wot/core");
const { HttpServer } = require("@node-wot/binding-http");

const tdFile = process.argv[2];
const port = parseInt(process.argv[3] || "8080", 10);
if (!tdFile) { console.error("Usage: node mock_server.js <td.json> [port]"); process.exit(1); }

const td = JSON.parse(fs.readFileSync(tdFile, "utf8"));
// node-wot generates the forms itself: drop the template ones and the security block
for (const p of Object.values(td.properties)) delete p.forms;
delete td.security; delete td.securityDefinitions;
td.title = ("mock-" + td.title).toLowerCase().replace(/[^a-z0-9]+/g, "-");

const store = {};
const servient = new Servient();
servient.addServer(new HttpServer({ port }));

servient.start().then(async (WoT) => {
  const thing = await WoT.produce(td);
  for (const name of Object.keys(td.properties)) {
    store[name] = null;
    thing.setPropertyReadHandler(name, async () => store[name]);
    thing.setPropertyWriteHandler(name, async (value) => {
      store[name] = await value.value();
      console.log(new Date().toISOString(), "WRITE", name, JSON.stringify(store[name]));
    });
  }
  await thing.expose();
  console.log(`Mock Thing exposed. Thing Description: http://localhost:${port}/${td.title}`);
});
