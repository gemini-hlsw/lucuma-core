// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.kernel.Eq
import lucuma.core.math.arb.*
import lucuma.core.model.SiderealTracking
import lucuma.core.model.arb.*
import lucuma.core.optics.syntax.lens.*
import monocle.law.discipline.LensTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

class DisjointZipSuite extends DisciplineSuite:
  import ArbCoordinates.given
  import ArbDeclination.given
  import ArbEpoch.given
  import ArbParallax.given
  import ArbProperMotion.given
  import ArbRadialVelocity.given
  import ArbRightAscension.given
  import ArbSiderealTracking.given

  val disjointZip2 = (SiderealTracking.baseCoordinates, SiderealTracking.epoch).disjointZip

  val disjointZip3 =
    (SiderealTracking.parallax, SiderealTracking.baseDec, SiderealTracking.radialVelocity).disjointZip

  val disjointZip4 =
    (SiderealTracking.baseRa,
      SiderealTracking.properMotion,
      SiderealTracking.radialVelocity,
      SiderealTracking.epoch
    ).disjointZip

  val disjointZip5 =
    (SiderealTracking.baseRa,
      SiderealTracking.baseDec,
      SiderealTracking.properMotion,
      SiderealTracking.parallax,
      SiderealTracking.epoch
    ).disjointZip

  checkAll("disjointZip2", LensTests(disjointZip2))
  checkAll("disjointZip3", LensTests(disjointZip3))
  checkAll("disjointZip4", LensTests(disjointZip4))
  checkAll("disjointZip5", LensTests(disjointZip5))
