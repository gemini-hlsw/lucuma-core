// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.ApertureExtent
import org.scalacheck.*

trait ArbApertureExtent {
  import ArbAngle.given

  // Real apertures are small and positive; the huge wrapped angles ArbAngle can
  // produce make no sense as an extent and would swamp the containment tests.
  private val genExtentAngle: Gen[Angle] =
    Gen.chooseNum(0L, 400_000_000L).map(Angle.fromMicroarcseconds)

  given Arbitrary[ApertureExtent] =
    Arbitrary {
      for {
        p <- genExtentAngle
        q <- genExtentAngle
      } yield ApertureExtent(p, q)
    }

  given Cogen[ApertureExtent] =
    Cogen[(Angle, Angle)].contramap(a => (a.p, a.q))

}

object ArbApertureExtent extends ArbApertureExtent
