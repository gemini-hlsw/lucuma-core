// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*
import lucuma.catalog.SEDMatcherFixture
import lucuma.core.enums.Band
import munit.CatsEffectSuite

class SimbadAdapterSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedMatcherFixture)

  test("be able to map brightness errors in Simbad") {
    val magErrorUcd = Ucd.unsafeFromString("stat.error;phot.mag")
    // FLUX_r maps to r'
    assertEquals(
      CatalogAdapter
        .Simbad(sedMatcher)
        .parseBrightnessValue(FieldId.unsafeFrom("FLUX_ERROR_r", magErrorUcd), "20.3051"),
      (FieldId.unsafeFrom("FLUX_ERROR_r", magErrorUcd), Band.SloanR, 20.3051).rightNec
    )

    // FLUX_R maps to R
    assertEquals(
      CatalogAdapter
        .Simbad(sedMatcher)
        .parseBrightnessValue(FieldId.unsafeFrom("FLUX_ERROR_R", magErrorUcd), "20.3051"),
      (FieldId.unsafeFrom("FLUX_ERROR_R", magErrorUcd), Band.R, 20.3051).rightNec
    )
  }
