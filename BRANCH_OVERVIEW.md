# SC-6518 SED Matching Implementation

## Entry Point: `SimbadSEDMatcher.inferSED`

**Location:** `modules/catalog/src/main/scala/lucuma/catalog/votable/SimbadSEDMatcher.scala:96`

**Main public API:**
```scala
def inferSED(
  otype:        String,                    // Simbad object type ("*", "G", "QSO", etc.)
  spectralType: Option[String] = None,    // e.g., "G2V", "K3III", "DA3"
  morphType:    Option[String] = None     // e.g., "Sa", "E3"
): EitherNec[CatalogProblem, UnnormalizedSED]
```

Returns either a matched SED or accumulated errors via `EitherNec[CatalogProblem, UnnormalizedSED]`.

---

## Key Components

### 1. Core Physics Calculations (`StellarPhysics.scala`)

Implements stellar physics calculations based on spectral type classification.

**Key Functions:**
- `spectralClassCode(spectralClass: String): Option[Double]` - Convert spectral letter + subclass to numerical code
  - Maps: O→0, B→10, A→20, F→30, G→40, K→50, M→60, L→70, T→80, Y→90
  - Handles decimals (G3.7), modifiers (+/-), defaults to 5 for bare letters
- `calculateTemperature(luminosityClasses, temperatureClasses): Option[Int]` - Effective temperature in Kelvin
  - Uses Malkov et al. 2020 polynomial for main sequence
  - Special handling for white dwarfs: T_eff = 50400 / number
- `calculateGravity(luminosityClasses, temperatureClasses): Option[Double]` - Surface gravity (log g)
  - Interpolates from Straizys & Kuriliene 1981 gravity table
  - Normalizes luminosity classes (I→Iab, VI→sd, drops subclasses)
- `calculateParameters(luminosityClasses, temperatureClasses): Option[StellarParameters]` - Combined calculation

**Type Safety:**
- Uses Coulomb quantities: `StellarParameters.tEff: Quantity[Int, Kelvin]`
- Temperature access via `.value` for arithmetic operations

### 2. Parser Combinators (`SpectralTypeParsers.scala`)

Robust parsing of astronomical spectral type strings using cats-parse.

**Key Parsers:**
- `tempClass: Parser[String]` - Temperature class: O9, G2, K3.5, M5+
- `lumClass: Parser[String]` - Luminosity class: V, III, Ia, IVb
- `tempRange: Parser[List[String]]` - Temperature ranges: G8/K0, M8-9
- `lumRange: Parser[List[String]]` - Luminosity ranges: V, IV/V, IIIb
- `whiteDwarf: Parser[(List[String], List[String])]` - White dwarfs: DA3.5, DBAP3
- `subdwarf: Parser[(List[String], List[String])]` - Subdwarfs: sdO2, sdG
- `mainSequence: Parser[(List[String], List[String])]` - Main sequence: G2V, K3III
- `spectralType: Parser[(List[String], List[String])]` - Master parser (tries WD → sd → main sequence)

**Returns:** `(luminosityClasses: List[String], temperatureClasses: List[String])`

### 3. SED Matching Logic (`SimbadSEDMatcher.scala`)

Main matching engine with object type routing and physics-based scoring.

**Object Type Routing (`inferSED`):**
1. **Stars** (`*`, `Ma*`, `WD*`, etc.) → `matchStellarSED` with physics-based scoring
2. **Galaxies** (`G`, `SBG`, etc.) → `matchGalaxySED` with morphology matching
3. **Quasars** (`QSO`, `BLL`, `AGN`, etc.) → Fixed: `UnnormalizedSED.Quasar(QuasarSpectrum.QS0)`
4. **HII Regions** (`HII`) → Fixed: `UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula)`
5. **Planetary Nebulae** (`PN`) → Fixed: `UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)`

**Stellar Matching (`matchStellarSED`, `findBestStellarMatch`):**
1. Parse spectral type string into luminosity and temperature classes
2. Calculate target star's physical parameters: T_eff and log(g)
3. Score all ~130 library SEDs using normalized distance in physics space:
   - Score = sqrt((ΔT/ΔT_max)² + (Δlog_g/Δlog_g_max)²)
   - Tolerances: 10% in T_eff, 0.5 dex in log(g)
4. Return best match (lowest score) if within both tolerance bounds

**Galaxy Matching (`matchGalaxySED`):**
- Elliptical patterns: `E[0-9:+]?.*` or `S0.*`
- Spiral patterns: `S[abcABC_:]?.*`
- Hubble stage classification: numeric range (-0.5 to 9.0)

**Type-Safe Scoring:**
- `ScoredMatch` case class encapsulates score, differences, and tolerances
- `isWithinTolerance: Boolean` method for filtering
- `scoreSpectrum` pure function for single spectrum evaluation

### 4. Error Handling

**Error Types (`CatalogProblem`):**
- `UnknownObjectType(otype: String)` - Simbad object type not recognized
- `InvalidSpectralType(spType: String)` - Spectral type string cannot be parsed
- `UnmatchedSpectralType(spType: String)` - Valid parse but no SED match within tolerance
- `InvalidMorphologicalType(morphType: String)` - Galaxy morphology not recognized

**Error Accumulation:**
- Uses `EitherNec` (Either with NonEmptyChain) for error accumulation
- Returns all problems at once, not just first failure

---

## Data Flow Example: "G2V" Star

```
inferSED("*", Some("G2V"), None)
  │
  ├─ parseObjectType("*") → Some(ObjectCategory.Star)
  │
  ├─ matchStellarSED("G2V")
  │   │
  │   ├─ parseSpectralType("G2V")
  │   │   └─ SpectralTypeParsers.spectralType.parse("G2V")
  │   │       → Some((List("V"), List("G2")))
  │   │
  │   ├─ findBestStellarMatch(List("V"), List("G2"))
  │   │   │
  │   │   ├─ StellarPhysics.calculateParameters(List("V"), List("G2"))
  │   │   │   ├─ spectralClassCode("G2") → Some(42.0)
  │   │   │   ├─ calculateTemperature(List("V"), List("G2"))
  │   │   │   │   └─ logTEff = 5.07073 - 7.57056e-2(42) + ... ≈ 3.716
  │   │   │   │       T_eff = 10^3.716 ≈ 5199 K
  │   │   │   ├─ calculateGravity(List("V"), List("G2"))
  │   │   │   │   └─ interpolateGravity(42.0, "V") ≈ 4.426 dex
  │   │   │   └─ Some(StellarParameters(5199 K, 4.426))
  │   │   │
  │   │   ├─ Score all ~130 library SEDs:
  │   │   │   ├─ G2V: ΔT=0, Δlog_g=0, score=0.0 ✓ (exact match)
  │   │   │   ├─ G1V: ΔT=∼100K, Δlog_g∼0.02, score≈0.1 ✓
  │   │   │   ├─ K2V: ΔT∼1000K, outside tolerance ✗
  │   │   │   └─ ...
  │   │   │
  │   │   └─ sortBy(_.score).headOption.filter(_.isWithinTolerance)
  │   │       → Some(ScoredMatch(G2V, 0.0, ...))
  │   │
  │   └─ map(_.spectrum) → Some(StellarLibrarySpectrum.G2V)
  │
  └─ Right(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G2V))
```

---

## File Structure

```
modules/catalog/src/main/scala/lucuma/catalog/votable/
├─ StellarPhysics.scala              # Core physics calculations
├─ SimbadSEDMatcher.scala            # Main entry point and routing
├─ SpectralTypeParsers.scala         # Parser combinators
└─ StellarLibraryParameters.scala    # Pre-calculated SED parameters

modules/catalog-tests/shared/src/test/scala/lucuma/catalog/votable/
├─ StellarPhysicsSuite.scala         # Unit tests for physics
├─ StellarPhysicsProperties.scala    # Property-based tests (ScalaCheck)
├─ SimbadSEDMatcherSuite.scala       # Unit tests for matcher
├─ SimbadSEDMatcherDataSuite.scala   # Data-driven tests (60 entries)
└─ SimbadSEDMatcherFullDataSuite.scala # Full dataset tests (8000+ entries)
```

---

## Recent Changes (This Branch)

### Commit: `48d262c8` - Refactor stellar matching to functional style

- Created `ScoredMatch` case class with `isWithinTolerance` method
- Extracted `scoreSpectrum` helper function for pure spectrum scoring
- Refactored `findBestStellarMatch` to functional pipeline:
  ```scala
  StellarLibrarySpectrum.values.toList
    .flatMap(scoreSpectrum(targetParams))
    .sortBy(_.score)
    .headOption
    .filter(_.isWithinTolerance)
    .map(_.spectrum)
  ```
- All 13 matcher tests passing

### Commit: `34d4f26a` - Use coulomb for physical parameters

- Changed `StellarParameters.tEff` from `Int` to `Quantity[Int, Kelvin]`
- Type-safe temperature representation with unit tracking
- Updated all scoring calculations to use `.value` accessor
- All 32 tests passing

### Commit: `cc2b5573` - Extract constants and add property-based tests

- Extracted galaxy morphological patterns to named constants
- Added 8 property-based tests with ScalaCheck:
  - Spectral codes increase with later types
  - Temperatures decrease with later types
  - Decimal subclasses update temperature
  - Modifiers (+/-) adjust spectral code
  - White dwarf formula validation
  - Brown dwarf rejection
  - Gravity constraints
- All 7245 tests passing

### Earlier Commits

- `eb8f49bd` - Improve SED matching: integrate parser, fix edge cases
- `9ea7d058` - Improve tests and integrate onto regular parser
- `c69f71b1` - Add heuristics to read SED from simbad
- `d3ee924a` - Cleanup: remove dead code
- `cb12090f` - scalafmt: format code

---

## Testing

### Unit Tests (SimbadSEDMatcherSuite)
- 13 tests covering object type classification, spectral parsing, morphology classification
- Example: verifying "G2V" matches `StellarLibrarySpectrum.G2V`

### Property-Based Tests (StellarPhysicsProperties)
- 8 ScalaCheck properties verifying physics constraints
- Example: temperature strictly decreases with later spectral types (O→B→A→...→M)

### Data-Driven Tests
- `SimbadSEDMatcherDataSuite`: 60 known Simbad objects (Sun, Vega, M87, etc.)
- `SimbadSEDMatcherFullDataSuite`: 8000+ entries from Python reference implementation

### Run Tests
```bash
sbt test                                    # All tests
sbt 'testOnly lucuma.catalog.votable.*'    # SED matching tests
sbt 'testOnly lucuma.catalog.votable.StellarPhysicsSuite'  # Physics only
```

---

## Key Physics References

- **Temperature**: Malkov et al., 2020, RAA, 20, 139 (polynomial calibration)
- **Gravity**: Straizys & Kuriliene, 1981, Ap&SS, 80, 353S (interpolation table)
- **Simbad**: https://simbad.u-strasbg.fr/Pages/guide/otypes.htx (object types)

---

## Next Steps (Optional)

Items from review not yet implemented:
- Python reference comparison tests (exact match validation)
- Performance benchmarks (<10ms per match)
- Module-level documentation with architecture overview
- Extended error context for debugging
