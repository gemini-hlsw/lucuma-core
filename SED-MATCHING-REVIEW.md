# Code Review: SED Matching from SIMBAD

## Overview

This branch implements automatic inference of Spectral Energy Distribution (SED) values from SIMBAD catalog data. The implementation translates stellar classifications (spectral types, morphological types, object types) into appropriate `UnnormalizedSED` values from the lucuma core model.

## Comparison with Python Reference (`match_sed.py`)

The Scala implementation is a faithful port of `match_sed.py` by Andrew Stephens. Both share:
- Identical object type classification sets
- Same temperature polynomial (Malkov et al. 2020)
- Same gravity interpolation (Straizys & Kuriliene 1981)
- Same scoring formula: `sqrt((ΔT/ΔT_max)² + (Δlog_g/Δlog_g_max)²)`

---

## Fixes Applied

### 1. E0 Elliptical Regex ✅
`E[1-9:]?` → `E[0-9:+]?` (also handles `E+` morphology)

### 2. Hubble Stage Threshold ✅
`< 0.0` → `<= -0.5` (matches Python)

### 3. G2V Fallbacks Removed ✅
Now returns `None` when no match found (matches Python behavior)

### 4. Luminosity Class Normalization ✅
Fixed contradictory cases to match Python logic

### 5. Spectral Type Decimals ✅
Parser now accepts any decimal (`.7`, `.3`, etc.) not just `.5`

---

## Test Results

All 34 tests pass:
- 13 unit tests in `SimbadSEDMatcherSuite`
- 13 data tests in `SimbadSEDMatcherDataSuite`
- 8 full dataset tests in `SimbadSEDMatcherFullDataSuite`

Match rates (without fallbacks - more accurate):
- Stars: 50% matched
- Galaxies: 54% matched
- Quasars: 100% matched

---

## Module Structure

- `SimbadSEDMatcher` - Main entry point
- `SpectralTypeParsers` - Parser combinators
- `StellarPhysics` - Physics calculations
- `StellarLibraryParameters` - Library SED parameters

Integration: `VoTableParser.scala:314-322` and `:365-373`
