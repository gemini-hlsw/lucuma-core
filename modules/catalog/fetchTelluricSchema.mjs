#!/usr/bin/env node
import { writeFile } from 'fs/promises';
import { buildClientSchema, getIntrospectionQuery, printSchema } from 'graphql';

const RED = '\x1b[0;31m';
const NC = '\x1b[0m';

const url = "https://telluric-targets.gpp.gemini.edu/";

const response = await fetch(new URL(url), {
  headers: {
    'Content-Type': 'application/json'
  },
  method: 'POST',
  body: JSON.stringify({ query: getIntrospectionQuery() }),
});

if (!response.ok) {
  throw new Error(`Failed to fetch introspection query: ${response.statusText}`);
}

console.log(`Fetched Telluric schema from ${url}`);

const data = (await response.json()).data;

const schema = printSchema(buildClientSchema(data));

const outputFile = 'src/clue/resources/lucuma/catalog/telluric/TelluricService.graphql';

await writeFile(outputFile, schema);
console.log(`Wrote Tellurics schema to ${outputFile}.`);
