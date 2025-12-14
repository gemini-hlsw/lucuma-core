# SED Matching Implementation Review

## Executive Summary

Reviewed the Scala SED matching implementation on branch `sc-6518-sed-matching2` against the Python reference implementation. Found **1 critical bug**, **multiple style deviations** from Lucuma patterns, and **testing gaps**. Overall algorithmic approach is sound and well-structured.

---

## Critical Issues

### 🔴 BUG: Incomplete Decimal Subclass Support (StellarPhysics.scala:44-48)

**Location:** `modules/catalog/src/main/scala/lucuma/catalog/votable/StellarPhysics.scala:44-48`

**Problem:**
```scala
val subclassPattern = """([0-9])(\.[05])?""".r  // ❌ Only matches .0 and .5
```

**Python Reference (match_sed.py:344):**
```python
subclass = re.search('[0-9](.5)?', spectral_class)  # Actually matches ANY decimal
# Then: subclass = float(subclass.group())  # Accepts 3.5, 3.7, etc.
```

**Impact:** Spectral types like "G3.7V", "K2.3III" will be parsed as "G3V", "K2III", causing incorrect temperature calculations.

**Python Test Evidence (match_sed.py:366):**
```python
assert spectral_class_code('G3.5') == 43.5  # This works
```

**Fix Required:** Change pattern to `"""([0-9])(\.[0-9]+)?""".r` to match any decimal

**Test Case to Add:**
```scala
assertEquals(StellarPhysics.spectralClassCode("G3.7"), Some(43.7))
assertEquals(StellarPhysics.spectralClassCode("K2.3"), Some(52.3))
```

---

## Functional Style Deviations from Lucuma Patterns

### 1. Missing Error Handling with EitherNec

**Current:**
```scala
def inferSED(...): Option[UnnormalizedSED]
```

**Lucuma Pattern (from CatalogProblem.scala):**
```scala
// Should accumulate errors instead of silently returning None
def inferSED(...): EitherNec[SEDMatchError, UnnormalizedSED]

sealed trait SEDMatchError extends Throwable with Product with Serializable:
  def displayValue: String

object SEDMatchError:
  case class UnknownObjectType(otype: String) extends SEDMatchError:
    val displayValue = s"Unknown object type: $otype"
  case class InvalidSpectralType(spType: String) extends SEDMatchError:
    val displayValue = s"Cannot parse spectral type: $spType"
  // etc.
```

**Why:** Silent failures make debugging difficult; error accumulation shows all problems at once.

### 2. Magic Numbers Not Extracted as Constants

**Current (SimbadSEDMatcher.scala:108-109):**
```scala
val dtMax = 0.1 * targetParams.tEff
val dgMax = 0.5
```

**Should Be:**
```scala
object StellarMatching:
  val TemperatureToleranceFraction: Double = 0.1  // 10% of target temperature
  val GravityToleranceDex: Double = 0.5           // 0.5 dex in log(g)
```

**Locations:** Lines 108-109, 287-288 (duplicated in tests)

### 3. Inline Regex Patterns Instead of Named Values

**Current (SimbadSEDMatcher.scala:137-140):**
```scala
val ellipticalPattern  = """E[0-9:+]?""".r
val s0Pattern          = """S0.*""".r
val spiralPattern      = """S[abcABC_:]?.*""".r
```

**Should Be:**
```scala
object GalaxyPatterns:
  val Elliptical: Regex = """E[0-9:+]?""".r
  val S0: Regex = """S0.*""".r
  val Spiral: Regex = """S[abcABC_:]?.*""".r
  val HubbleStage: Regex = """-?[0-9]+\.?[0-9]*""".r
```

### 4. No Type Safety for Physical Parameters

**Current:**
```scala
case class StellarParameters(tEff: Int, logG: Double)
```

**Lucuma Pattern (should use refined types):**
```scala
import eu.timepit.refined.*
import eu.timepit.refined.numeric.*

type EffectiveTemperature = Int Refined Positive
type SurfaceGravity = Double Refined Interval.Closed[0.0, 10.0]

case class StellarParameters(
  tEff: EffectiveTemperature,
  logG: SurfaceGravity
)
```

### 5. Missing ScalaDoc on Public API

**Current:**
```scala
def inferSED(otype: String, spectralType: Option[String], morphType: Option[String]): Option[UnnormalizedSED]
```

**Should Have:**
```scala
/**
 * Infer an appropriate UnnormalizedSED from Simbad object classification.
 *
 * Uses physics-based matching for stars (effective temperature and surface gravity)
 * and morphological patterns for galaxies. Based on the reference implementation
 * by Andrew Stephens (match_sed.py).
 *
 * @param otype Simbad object type code (e.g., "*", "G", "QSO")
 * @param spectralType Optional spectral classification (e.g., "G2V", "K3III")
 * @param morphType Optional morphological type for galaxies (e.g., "Sa", "E3")
 * @return Matched SED or None if no suitable match found
 *
 * @see [[https://simbad.u-strasbg.fr/Pages/guide/otypes.htx Simbad Object Types]]
 * @see [[https://simbad.u-strasbg.fr/Pages/guide/chD.htx Simbad Spectral Types]]
 */
```

---

## Tricky Parts & Potential Bugs

### 1. Parser Backtracking Order Matters (SpectralTypeParsers.scala:140)

```scala
val spectralType: Parser[(List[String], List[String])] =
  (whiteDwarf.backtrack | subdwarf.backtrack | mainSequence)
```

**Issue:** Order is critical. If changed, could mis-parse types.

**Example:** `"sdB1"` could match mainSequence as `([], ["s", "d", "B1"])` if tried first.

**Recommendation:** Add explicit test for parser priority:
```scala
test("parser should try white dwarf before main sequence") {
  assertEquals(parseSpectralType("DA3"), Some((List("DA"), List("3"))))
}
test("parser should try subdwarf before main sequence") {
  assertEquals(parseSpectralType("sdB1"), Some((List("sd"), List("B1"))))
}
```

### 2. Lenient Subdwarf Parsing May Be Too Permissive (SpectralTypeParsers.scala:111)

```scala
private val lenientTempClass: Parser[String] =
  (charIn("OBAFGKMLTYABCNPXZ") ~ (digit | charIn("OBAFGKMLTYABCNPXZ.+-")).rep0).string
```

**Issue:** Could match unexpected patterns. The Python version is more restrictive:
```python
t_class = re.search('[OBAFGKMLTY][0-9]?[.]?[0-9]?[+-]?[-/]?', sp_type).group()
```

**Risk:** May accept invalid subdwarf types that Python would reject.

**Test Coverage Needed:**
- Valid: `sdO2`, `sdB1`, `sdG`
- Invalid: `sdXYZ`, `sd123`, `sdABCD`

### 3. Galaxy Hubble Stage Parsing Has Edge Case (SimbadSEDMatcher.scala:149-158)

```scala
case hubbleStagePattern() =>
  val hubbleStage = morphType.toDoubleOption.getOrElse(0.0)  // ⚠️ Defaults to 0.0
```

**Problem:** If `toDoubleOption` fails, silently defaults to 0.0 (Spiral), but should return None.

**Python Behavior (match_sed.py:245):**
```python
hubble_stage = float(morph_type)  # Throws exception if invalid
```

**Fix:**
```scala
case hubbleStagePattern() =>
  morphType.toDoubleOption.flatMap { hubbleStage =>
    if (hubbleStage <= -0.5) Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    else if (hubbleStage < 9) Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
    else None
  }
```

### 4. Spectral Class Code Defaults to 5.0 (StellarPhysics.scala:51)

```scala
case _ => 5.0 // Default to 5 if no subclass specified
```

**Python Reference (match_sed.py:347):**
```python
if subclass is None:
    subclass = 5.
    logger.info('Spectral class not fully specified; %s -> %s%d', spectral_class, letter, subclass)
```

**Observation:** This matches Python but is undocumented. Add comment explaining this is intentional (e.g., "G" → "G5").

### 5. Gravity Interpolation Edge Cases (StellarPhysics.scala:240-248)

```scala
val (lower, upper) = gravityTable.partition(_._1 <= scc) match
  case (lows, highs) if lows.nonEmpty && highs.nonEmpty => (lows.last, highs.head)
  case (lows, _) if lows.nonEmpty => (lows.last, lows.last) // Extrapolate
  case (_, highs) if highs.nonEmpty => (highs.head, highs.head) // Extrapolate
  case _ => return 4.0 // Default
```

**Issue:** Extrapolation behavior differs from Python's `numpy.interp`, which extends with edge values.

**Python (match_sed.py:477):**
```python
numpy.interp(scc, t['Sc'][not_masked], t[lc][not_masked])
```

`numpy.interp` extrapolates flat (returns edge value) outside range, while Scala duplicates edge entry for interpolation (same result, different mechanism).

**Verdict:** Functionally equivalent, but add test for extrapolation:
```scala
test("gravity extrapolation for late M-type stars") {
  // M9.5V (scc=69.5) is at table edge
  val result = StellarPhysics.calculateGravity(List("V"), List("M9.5"))
  assert(result.isDefined)
}
```

---

## Testing Gaps

### 1. Missing Property-Based Tests

**Current:** Only example-based tests (SimbadSEDMatcherSuite.scala)

**Should Add (using ScalaCheck):**
```scala
import org.scalacheck.Prop.*
import munit.ScalaCheckSuite

class StellarPhysicsProperties extends ScalaCheckSuite:
  property("spectral class code increases with later spectral types") {
    forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M")) { letter1 =>
      forAll(Gen.oneOf("O", "B", "A", "F", "G", "K", "M")) { letter2 =>
        val ordering = List("O", "B", "A", "F", "G", "K", "M")
        if ordering.indexOf(letter1) < ordering.indexOf(letter2) then
          val scc1 = StellarPhysics.spectralClassCode(letter1 + "0")
          val scc2 = StellarPhysics.spectralClassCode(letter2 + "0")
          assert(scc1.get < scc2.get)
      }
    }
  }

  property("temperature decreases with later spectral types") {
    forAll(Gen.choose(0.0, 60.0)) { scc1 =>
      forAll(Gen.choose(scc1 + 1.0, 60.0)) { scc2 =>
        // Later spectral types (higher scc) should have lower temperatures
        // (polynomial is decreasing function)
      }
    }
  }
```

### 2. Missing Edge Case Tests

**Add These:**
```scala
test("empty spectral type should return None") {
  assertEquals(SimbadSEDMatcher.inferSED("*", Some("")), None)
}

test("whitespace-only spectral type") {
  assertEquals(SimbadSEDMatcher.inferSED("*", Some("   ")), None)
}

test("spectral type with only uncertainty markers") {
  assertEquals(parseSpectralType(":::"), None)
}

test("temperature class with non-.5 decimal") {
  val result = parseSpectralType("G3.7V")
  assert(result.isDefined)
  assertEquals(result.get._2, List("G3.7"))  // Should parse decimal
}

test("multiple slashes in temperature range") {
  val result = parseSpectralType("F8/G0/G2V")
  assert(result.isDefined)
  // Should handle gracefully
}

test("brown dwarf spectral types should return None") {
  assertEquals(StellarPhysics.calculateTemperature(List("V"), List("L5")), None)
  assertEquals(StellarPhysics.calculateTemperature(List("V"), List("T8")), None)
  assertEquals(StellarPhysics.calculateTemperature(List("V"), List("Y0")), None)
}

test("white dwarf with invalid temperature") {
  // What happens with "DA?" or "DAX"?
  val result = StellarPhysics.calculateTemperature(List("DA"), List("?"))
  // Should return None or handle gracefully
}
```

### 3. Missing Negative Tests

**Add:**
```scala
test("unknown object type returns None") {
  assertEquals(SimbadSEDMatcher.inferSED("???", None, None), None)
  assertEquals(SimbadSEDMatcher.inferSED("UNKNOWN", Some("G2V"), None), None)
}

test("galaxy with invalid morphological type") {
  assertEquals(SimbadSEDMatcher.inferSED("G", None, Some("InvalidMorph")), None)
}
```

### 4. Missing Comparison Tests with Python Output

**Should Add (using test data file):**
```scala
test("matches Python reference implementation on benchmark dataset") {
  // Load simbad-sed-test-data.dat
  // For each row, compare Scala output to Python 'filename' column
  // Assert match rate >= 95%
}
```

**Currently exists but not strict enough:** SimbadSEDMatcherDataSuite.scala checks existence, not exact matches.

---

## Style Improvements

### 1. Consistent Naming Conventions

**Issue:** Mixed naming styles for object categories

**Current:**
```scala
val StarTypes = Set(...)    // Plural
val GalaxyTypes = Set(...)  // Plural
val QuasarTypes = Set(...)  // Plural
val HIITypes = Set(...)     // Caps
val PNTypes = Set(...)      // Caps
```

**Should Be:**
```scala
object SimbadObjectTypes:
  val Star: Set[String] = Set(...)
  val Galaxy: Set[String] = Set(...)
  val Quasar: Set[String] = Set(...)
  val HIIRegion: Set[String] = Set(...)
  val PlanetaryNebula: Set[String] = Set(...)
```

### 2. Extract Object Category Parsing

**Current:** Inline logic in `parseObjectType` (lines 55-60)

**Better:**
```scala
private val categoryMappings: List[(Set[String], ObjectCategory)] = List(
  (StarTypes, ObjectCategory.Star),
  (GalaxyTypes, ObjectCategory.Galaxy),
  (QuasarTypes, ObjectCategory.Quasar),
  (HIITypes, ObjectCategory.HIIRegion),
  (PNTypes, ObjectCategory.PlanetaryNebula)
)

private def parseObjectType(otype: String): Option[ObjectCategory] =
  categoryMappings.collectFirst {
    case (types, category) if types.contains(otype) => category
  }
```

### 3. Functional Pattern for Stellar Matching

**Current (lines 103-116):**
```scala
val scored = StellarLibrarySpectrum.values.toList.flatMap { spectrum =>
  StellarLibraryParameters.getParameters(spectrum).map { sedParams =>
    val dt = sedParams.tEff - targetParams.tEff
    val dg = sedParams.logG - targetParams.logG
    // ... calculations
    (spectrum, score, math.abs(dt), dtMax, math.abs(dg), dgMax)
  }
}
```

**More Functional:**
```scala
case class ScoredMatch(
  spectrum: StellarLibrarySpectrum,
  score: Double,
  absDt: Double,
  dtMax: Double,
  absDg: Double,
  dgMax: Double
):
  def isWithinTolerance: Boolean = absDt < dtMax && absDg < dgMax

val scored = StellarLibrarySpectrum.values.toList.flatMap { spectrum =>
  StellarLibraryParameters.getParameters(spectrum).map { sedParams =>
    scoreMatch(targetParams, sedParams, spectrum)
  }
}

scored.filter(_.isWithinTolerance).minByOption(_.score).map(_.spectrum)
```

### 4. Use Extension Methods

**Current:**
```scala
if temperatureClasses.isEmpty && luminosityClasses.isEmpty then None
```

**Lucuma Pattern:**
```scala
extension (classes: List[String])
  def nonEmptyOption: Option[List[String]] =
    if classes.isEmpty then None else Some(classes)

(temperatureClasses.nonEmptyOption, luminosityClasses.nonEmptyOption).tupled.flatMap { ... }
```

---

## Suggested Additional Tests

### Unit Tests to Add

1. **Spectral Class Code:**
   - Test all letter conversions (O, B, A, F, G, K, M, L, T, Y)
   - Test decimal subclasses (3.1, 3.2, ..., 3.9, not just 3.5)
   - Test +/- modifiers with decimals ("K3.5+", "G2.7-")

2. **Temperature Calculation:**
   - Test white dwarfs with various numbers (DA1, DA5, DA10)
   - Test subdwarfs with different temperature classes
   - Test averaging multiple temperature classes
   - Test boundary cases (O0, M9.5)

3. **Gravity Calculation:**
   - Test all luminosity class normalizations (I→Iab, VI→sd, IIIa→III)
   - Test interpolation at table boundaries
   - Test averaging multiple classes

4. **Parser Combinators:**
   - Test all parser error cases
   - Test parser with malformed input
   - Test parser with trailing garbage
   - Verify `.parse()` vs `.parseAll()` behavior

5. **Galaxy Matching:**
   - Test all morphological type patterns
   - Test Hubble stage boundaries (-0.5, 9.0)
   - Test invalid Hubble stages

6. **Integration:**
   - Test SED inference with missing data (None for spectralType/morphType)
   - Test SED inference with contradictory data
   - Test round-trip: OTYPE → SED → tag should be reasonable

### Performance Tests

```scala
test("SED matching should complete in <10ms for typical input") {
  val start = System.nanoTime()
  val result = SimbadSEDMatcher.inferSED("*", Some("G2V"))
  val elapsed = (System.nanoTime() - start) / 1e6
  assert(elapsed < 10.0, s"Matching took ${elapsed}ms")
}
```

---

## Documentation Improvements

### 1. Add Module-Level Documentation

**File:** `SimbadSEDMatcher.scala`

```scala
/**
 * Automatic SED (Spectral Energy Distribution) inference from Simbad catalog data.
 *
 * This module translates astronomical object classifications from the Simbad database
 * into appropriate SEDs from the Lucuma spectral library. The matching algorithm uses:
 *
 * - **Stars**: Physics-based matching using effective temperature and surface gravity
 *   calculated from spectral types (e.g., "G2V"). Tolerances: 10% in T_eff, 0.5 dex in log(g).
 *
 * - **Galaxies**: Pattern matching on morphological types (e.g., "Sa", "E3") or
 *   numerical Hubble stage classification.
 *
 * - **Quasars/AGN**: Fixed spectrum (QS0)
 * - **HII Regions**: Fixed spectrum (Orion Nebula)
 * - **Planetary Nebulae**: Fixed spectrum (NGC7009)
 *
 * == Algorithm References ==
 *
 * Based on the Python reference implementation by Andrew Stephens (match_sed.py).
 *
 * Stellar physics:
 *  - Temperature: Malkov et al, 2020, RAA, 20, 139
 *  - Gravity: Straizys & Kuriliene, 1981, Ap&SS, 80, 353S
 *
 * @see [[https://simbad.u-strasbg.fr Simbad Astronomical Database]]
 */
```

### 2. Add Algorithm Explanation Comments

**In `findBestStellarMatch`:**
```scala
// Scoring algorithm:
// We calculate a normalized distance in (T_eff, log_g) parameter space.
// The distance is normalized by tolerances: 10% in temperature, 0.5 dex in gravity.
// This creates an "acceptance ellipse" - only matches within both tolerances are valid.
// Score = sqrt((ΔT/ΔT_max)² + (Δlog_g/Δlog_g_max)²)
// The lowest-scoring match within tolerance is selected.
```

### 3. Add Inline Examples

```scala
/**
 * Parse Simbad spectral type string.
 *
 * @example
 * {{{
 * parseSpectralType("G2V")        // Some((List("V"), List("G2")))
 * parseSpectralType("K1/2III")    // Some((List("III"), List("K1", "K2")))
 * parseSpectralType("DA3.5")      // Some((List("DA"), List("3.5")))
 * parseSpectralType("")           // None
 * }}}
 */
```

---

## Recommendations

### High Priority (Must Fix)

1. **Fix decimal subclass bug** in `StellarPhysics.spectralClassCode` (change regex pattern)
2. **Fix galaxy Hubble stage default** in `matchGalaxySED` (don't default to 0.0)
3. **Add edge case tests** for decimal subclasses, brown dwarfs, invalid inputs

### Medium Priority (Should Fix)

1. **Extract magic numbers** as named constants
2. **Add ScalaDoc** to public API methods
3. **Add property-based tests** for physics calculations
4. **Use EitherNec** for error handling instead of Option
5. **Extract regex patterns** to named values

### Low Priority (Nice to Have)

1. **Use refined types** for physical parameters
2. **Add module-level documentation**
3. **Refactor to more functional style** with case classes for scored matches
4. **Add performance tests**
5. **Create comparison test** against full Python output

---

## Files Requiring Changes

### Core Implementation
1. `modules/catalog/src/main/scala/lucuma/catalog/votable/StellarPhysics.scala` - **BUG FIX** + constants
2. `modules/catalog/src/main/scala/lucuma/catalog/votable/SimbadSEDMatcher.scala` - Galaxy fix + refactoring
3. `modules/catalog/src/main/scala/lucuma/catalog/votable/SpectralTypeParsers.scala` - Documentation

### Testing
4. `modules/catalog-tests/shared/src/test/scala/lucuma/catalog/votable/SimbadSEDMatcherSuite.scala` - Add edge cases
5. `modules/catalog-tests/shared/src/test/scala/lucuma/catalog/votable/StellarPhysicsSuite.scala` - **NEW FILE** for unit tests
6. `modules/catalog-tests/shared/src/test/scala/lucuma/catalog/votable/StellarPhysicsProperties.scala` - **NEW FILE** for property tests

---

## Summary

The Scala implementation is **largely correct** and follows the Python algorithm closely. The main issues are:

- ✅ **Algorithm**: Sound, matches Python reference
- ⚠️ **Correctness**: 1 critical bug (decimal subclasses), 1 minor bug (Hubble stage default)
- ⚠️ **Style**: Deviates from Lucuma patterns (needs refinement types, error handling, constants)
- ⚠️ **Testing**: Good coverage but missing edge cases, property tests, and negative tests
- ⚠️ **Documentation**: Minimal but adequate; could benefit from ScalaDoc and examples

**Estimated effort to address all issues:** 3-4 hours (1 hour bugs, 2 hours style, 1 hour tests)
