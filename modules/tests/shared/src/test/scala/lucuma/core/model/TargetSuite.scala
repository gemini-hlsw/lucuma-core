// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class TargetSuite extends DisciplineSuite {
  import ArbTarget._
  import ArbMagnitude._
  import ArbSiderealTracking._
  import ArbEphemerisKey._
  import ArbParallax._
  import ArbEnumerated._
  import ArbCoordinates._
  import ArbRightAscension._
  import ArbDeclination._
  import ArbProperMotion._
  import ArbRadialVelocity._

  // Laws
  checkAll("Eq[Target]", EqTests[Target].eqv)
  checkAll("TargetTrack", OrderTests[Target](Target.TargetTrackOrder).order)
  checkAll("TargetName", OrderTests[Target](Target.TargetNameOrder).order)
  checkAll("Target.name", LensTests(Target.name))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.ephemerisKey", OptionalTests(Target.ephemerisKey))
  checkAll("Target.magnitudes", LensTests(Target.magnitudes))
  checkAll("Target.magnitudesT", TraversalTests(Target.magnitudesT))
  checkAll("Target.parallax", OptionalTests(Target.parallax))
  checkAll("Target.radialVelocity", OptionalTests(Target.radialVelocity))
  checkAll("Target.baseCoordinates", OptionalTests(Target.baseCoordinates))
  checkAll("Target.baseRA", OptionalTests(Target.baseRA))
  checkAll("Target.baseDec", OptionalTests(Target.baseDec))
  checkAll("Target.siderealTracking", OptionalTests(Target.siderealTracking))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.properMotionRA", OptionalTests(Target.properMotionRA))
  checkAll("Target.properMotionDec", OptionalTests(Target.properMotionDec))
  checkAll("Target.magnitudeInBand", TraversalTests(Target.magnitudeIn(MagnitudeBand.B)))
}
