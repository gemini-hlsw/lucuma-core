// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.tests.CatsSuite
import lucuma.core.optics.Spire._
import spire.laws.arb._
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.optics.laws.discipline.SplitEpiTests
import org.scalacheck.Arbitrary._
import monocle.law.discipline.IsoTests

final class SpireOpticsSpec extends CatsSuite {

  checkAll("numberInt", SplitEpiTests(numberInt).splitEpi)
  checkAll("numberLong", SplitEpiTests(numberLong).splitEpi)
  checkAll("numberFloat", SplitEpiTests(numberFloat).splitEpi)
  checkAll("numberDouble", SplitEpiTests(numberDouble).splitEpi)
  checkAll("numberBigInt", SplitEpiTests(numberBigInt).splitEpi)
  checkAll("numberSafeLong", SplitEpiTests(numberSafeLong).splitEpi)
  checkAll("numberBigDecimal", IsoTests(numberBigDecimal))
  // Number <-> Rational is actually an Iso in the JVM and a SplitEpi in JS, because of Long precision.
  // checkAll("numberRational", SplitEpiTests(numberRational).splitEpi)
  // checkAll("numberRational", IsoTests(numberRational))
  checkAll("numberNatural", FormatTests(numberNatural).format)
}
