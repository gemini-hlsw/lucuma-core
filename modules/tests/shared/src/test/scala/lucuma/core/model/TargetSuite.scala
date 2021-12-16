// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import monocle.law.discipline._
import munit._

final class TargetSuite extends DisciplineSuite {
  import ArbTarget._
  import ArbSourceProfile._
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

  // Laws for Target.Sidereal
  checkAll("Eq[Target.Sidereal]", EqTests[Target.Sidereal].eqv)
  checkAll(
    "Target.Sidereal.TrackOrder",
    OrderTests[Target.Sidereal](Target.Sidereal.TrackOrder).order
  )
  checkAll(
    "Target.Sidereal.NameOrder",
    OrderTests[Target.Sidereal](Target.Sidereal.NameOrder).order
  )
  checkAll("Target.Sidereal.name", LensTests(Target.Sidereal.name))
  checkAll("Target.Sidereal.tracking", LensTests(Target.Sidereal.tracking))
  checkAll("Target.Sidereal.properMotion", OptionalTests(Target.Sidereal.properMotion))
  checkAll("Target.Sidereal.sourceProfile", LensTests(Target.Sidereal.sourceProfile))
  checkAll("Target.Sidereal.parallax", LensTests(Target.Sidereal.parallax))
  checkAll("Target.Sidereal.radialVelocity", LensTests(Target.Sidereal.radialVelocity))
  checkAll("Target.Sidereal.baseCoordinates", LensTests(Target.Sidereal.baseCoordinates))
  checkAll("Target.Sidereal.baseRA", LensTests(Target.Sidereal.baseRA))
  checkAll("Target.Sidereal.baseDec", LensTests(Target.Sidereal.baseDec))
  checkAll("Target.Sidereal.catalogId", LensTests(Target.Sidereal.catalogId))
  checkAll("Target.Sidereal.epoch", LensTests(Target.Sidereal.epoch))
  checkAll("Target.Sidereal.properMotion", LensTests(Target.Sidereal.properMotion))
  checkAll("Target.Sidereal.properMotionRA", OptionalTests(Target.Sidereal.properMotionRA))
  checkAll("Target.Sidereal.properMotionDec", OptionalTests(Target.Sidereal.properMotionDec))

  // Laws for Target.Nonsidereal
  checkAll("Eq[Target.Nonsidereal]", EqTests[Target.Nonsidereal].eqv)
  checkAll(
    "Target.Nonsidereal.TrackOrder",
    OrderTests[Target.Nonsidereal](Target.Nonsidereal.TrackOrder).order
  )
  checkAll(
    "Target.Nonsidereal.NameOrder",
    OrderTests[Target.Nonsidereal](Target.Nonsidereal.NameOrder).order
  )
  checkAll("Target.Nonsidereal.name", LensTests(Target.Nonsidereal.name))
  checkAll("Target.Nonsidereal.ephemerisKey", LensTests(Target.Nonsidereal.ephemerisKey))
  checkAll("Target.Nonsidereal.sourceProfile", LensTests(Target.Nonsidereal.sourceProfile))

  // Laws for Target
  checkAll("Target.Id", GidTests[Target.Id].gid)
  checkAll("Eq[Target]", EqTests[Target].eqv)
  checkAll("Target.TrackOrder", OrderTests[Target](Target.TrackOrder).order)
  checkAll("Target.NameOrder", OrderTests[Target](Target.NameOrder).order)
  checkAll("Target.name", LensTests(Target.name))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.ephemerisKey", OptionalTests(Target.ephemerisKey))
  checkAll("Target.sourceProfile", LensTests(Target.sourceProfile))
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
}
