#!/bin/bash

pnpm install --frozen-lockfile

./fetchTelluricSchema.mjs $@
