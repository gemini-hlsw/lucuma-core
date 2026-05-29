// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.arb.ArbSiderealTracking
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGhostIfuMapping:

  import ArbCoordinates.given
  import ArbSiderealTracking.given

  import GhostIfuMapping.*

  given Arbitrary[Nonsidereal.type] =
    Arbitrary:
      Gen.const(Nonsidereal)

  given Arbitrary[SingleTarget] =
    Arbitrary:
      arbitrary[SiderealTracking].map(SingleTarget(_))

  given Cogen[SingleTarget] =
    Cogen[SiderealTracking].contramap(_.ifu1)

  given Arbitrary[TargetPlusSky] =
    Arbitrary:
      for
        t <- arbitrary[SiderealTracking]
        c <- arbitrary[Coordinates]
      yield TargetPlusSky(t, c)

  given Cogen[TargetPlusSky] =
    Cogen[(SiderealTracking, Coordinates)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[SkyPlusTarget] =
    Arbitrary:
      for
        c <- arbitrary[Coordinates]
        t <- arbitrary[SiderealTracking]
      yield SkyPlusTarget(c, t)

  given Cogen[SkyPlusTarget] =
    Cogen[(Coordinates, SiderealTracking)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[DualTarget] =
    Arbitrary:
      for
        t0 <- arbitrary[SiderealTracking]
        t1 <- arbitrary[SiderealTracking]
      yield DualTarget(t0, t1)

  given Cogen[DualTarget] =
    Cogen[(SiderealTracking, SiderealTracking)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[GhostIfuMapping] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[Nonsidereal.type],
        arbitrary[SingleTarget],
        arbitrary[TargetPlusSky],
        arbitrary[SkyPlusTarget],
        arbitrary[DualTarget]
      )

  given Cogen[GhostIfuMapping] =
    Cogen[(
      Int,
      Option[SiderealTracking],
      Option[(SiderealTracking, Coordinates)],
      Option[(Coordinates, SiderealTracking)],
      Option[(SiderealTracking, SiderealTracking)]
    )].contramap:
      case Nonsidereal         => (0, None, None, None, None)
      case SingleTarget(t)     => (1, Some(t), None, None, None)
      case TargetPlusSky(t, c) => (2, None, Some((t, c)), None, None)
      case SkyPlusTarget(c, t) => (3, None, None, Some((c, t)), None)
      case DualTarget(t0, t1)  => (4, None, None, None, Some((t0, t1)))

object ArbGhostIfuMapping extends ArbGhostIfuMapping