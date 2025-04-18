// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.cats.quantity.given
import coulomb.syntax.*
import lucuma.core.math.arb.*
import lucuma.core.math.units.Picometer
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*

class WavelengthDitherSuite extends DisciplineSuite {
  import ArbQuantity.given
  import ArbWavelengthDither.given

  checkAll("picometers",  IsoTests(WavelengthDither.picometers))
  checkAll("angstroms",   FormatTests(WavelengthDither.angstroms).format)
  checkAll("nanometers",  FormatTests(WavelengthDither.nanometers).format)
  checkAll("micrometers", FormatTests(WavelengthDither.micrometers).format)

  checkAll("intPicometers",      IsoTests(WavelengthDither.intPicometers))
  checkAll("decimalAngstroms",   FormatTests(WavelengthDither.decimalAngstroms).format)
  checkAll("decimalNanometers",  FormatTests(WavelengthDither.decimalNanometers).format)
  checkAll("decimalMicrometers", FormatTests(WavelengthDither.decimalMicrometers).format)

  property("angstroms") {
    forAll { (wd: WavelengthDither) =>
      assertEquals(
        wd.toPicometers.value,
        (wd.toAngstroms.value * 100).bigDecimal.intValueExact
      )
    }
  }

  property("nanometers") {
    forAll { (wd: WavelengthDither) =>
      assertEquals(
        wd.toPicometers.value,
        (wd.toNanometers.value * 1_000).bigDecimal.intValueExact
      )
    }
  }

  property("micrometers") {
    forAll { (wd: WavelengthDither) =>
      assertEquals(
        wd.toPicometers.value,
        (wd.toMicrometers.value * 1_000_000).bigDecimal.intValueExact
      )
    }
  }

  property("abs") {
    forAll { (wd: WavelengthDither) =>
      if (wd > WavelengthDither.Zero) assertEquals(wd, wd.abs)
      else assertEquals(WavelengthDither(wd.toPicometers.value.abs.withUnit[Picometer]), wd.abs)
    }
  }
}
