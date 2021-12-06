// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.enum.Band
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import monocle.law.discipline._
import munit._

final class TargetSuite extends DisciplineSuite {
  import ArbTarget._
  import ArbTargetBrightness._
  import ArbSiderealTracking._
  import ArbEphemerisKey._
  import ArbParallax._
  import ArbEnumerated._
  import ArbCoordinates._
  import ArbRightAscension._
  import ArbDeclination._
  import ArbProperMotion._
  import ArbRadialVelocity._
  import ArbGid._
  import ArbEpoch._
  import ArbCatalogId._
  import Target._

  // Laws for SiderealTarget
  checkAll("Eq[SiderealTarget]", EqTests[SiderealTarget].eqv)
  checkAll("SiderealTarget.TrackOrder", OrderTests[SiderealTarget](SiderealTarget.TrackOrder).order)
  checkAll("SiderealTarget.NameOrder", OrderTests[SiderealTarget](SiderealTarget.NameOrder).order)
  checkAll("SiderealTarget.name", LensTests(SiderealTarget.name))
  checkAll("SiderealTarget.tracking", LensTests(SiderealTarget.tracking))
  checkAll("SiderealTarget.properMotion", OptionalTests(SiderealTarget.properMotion))
  checkAll("SiderealTarget.brightnesses", LensTests(SiderealTarget.brightnesses))
  checkAll("SiderealTarget.brightnessesT", TraversalTests(SiderealTarget.brightnessesT))
  checkAll("SiderealTarget.parallax", LensTests(SiderealTarget.parallax))
  checkAll("SiderealTarget.radialVelocity", LensTests(SiderealTarget.radialVelocity))
  checkAll("SiderealTarget.baseCoordinates", LensTests(SiderealTarget.baseCoordinates))
  checkAll("SiderealTarget.baseRA", LensTests(SiderealTarget.baseRA))
  checkAll("SiderealTarget.baseDec", LensTests(SiderealTarget.baseDec))
  checkAll("SiderealTarget.catalogId", LensTests(SiderealTarget.catalogId))
  checkAll("SiderealTarget.epoch", LensTests(SiderealTarget.epoch))
  checkAll("SiderealTarget.properMotion", LensTests(SiderealTarget.properMotion))
  checkAll("SiderealTarget.properMotionRA", OptionalTests(SiderealTarget.properMotionRA))
  checkAll("SiderealTarget.properMotionDec", OptionalTests(SiderealTarget.properMotionDec))
  checkAll(
    "SiderealTarget.brightnessIn",
    TraversalTests(SiderealTarget.brightnessIn(Band.B))
  )

  // Laws for NonsiderealTarget
  checkAll("Eq[NonsiderealTarget]", EqTests[NonsiderealTarget].eqv)
  checkAll(
    "NonsiderealTarget.TrackOrder",
    OrderTests[NonsiderealTarget](NonsiderealTarget.TrackOrder).order
  )
  checkAll(
    "NonsiderealTarget.NameOrder",
    OrderTests[NonsiderealTarget](NonsiderealTarget.NameOrder).order
  )
  checkAll("NonsiderealTarget.name", LensTests(NonsiderealTarget.name))
  checkAll("NonsiderealTarget.ephemerisKey", LensTests(NonsiderealTarget.ephemerisKey))
  checkAll("NonsiderealTarget.brightnesses", LensTests(NonsiderealTarget.brightnesses))
  checkAll("NonsiderealTarget.brightnessesT", TraversalTests(NonsiderealTarget.brightnessesT))

  // Laws for Target
  checkAll("Target.Id", GidTests[Target.Id].gid)
  checkAll("Eq[Target]", EqTests[Target].eqv)
  checkAll("Target.TrackOrder", OrderTests[Target](Target.TrackOrder).order)
  checkAll("Target.NameOrder", OrderTests[Target](Target.NameOrder).order)
  checkAll("Target.name", LensTests(Target.name))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.ephemerisKey", OptionalTests(Target.ephemerisKey))
  checkAll("Target.brightnesses", LensTests(Target.brightnesses))
  checkAll("Target.brightnessesT", TraversalTests(Target.brightnessesT))
  checkAll("Target.sidereal", PrismTests(Target.sidereal))
  checkAll("Target.nonsidereal", PrismTests(Target.nonsidereal))
  checkAll("Target.siderealTracking", OptionalTests(Target.siderealTracking))
  checkAll("Target.parallax", OptionalTests(Target.parallax))
  checkAll("Target.radialVelocity", OptionalTests(Target.radialVelocity))
  checkAll("Target.baseCoordinates", OptionalTests(Target.baseCoordinates))
  checkAll("Target.baseRA", OptionalTests(Target.baseRA))
  checkAll("Target.baseDec", OptionalTests(Target.baseDec))
  checkAll("Target.catalogId", OptionalTests(Target.catalogId))
  checkAll("Target.epoch", OptionalTests(Target.epoch))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.properMotionRA", OptionalTests(Target.properMotionRA))
  checkAll("Target.properMotionDec", OptionalTests(Target.properMotionDec))
  checkAll("Target.brightnessIn", TraversalTests(Target.brightnessIn(Band.B)))
}
