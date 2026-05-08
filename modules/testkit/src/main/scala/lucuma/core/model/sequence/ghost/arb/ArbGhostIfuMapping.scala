// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGhostIfuMapping:

  import ArbCoordinates.given
  import ArbTarget.given

  import GhostIfuMapping.*

  given Arbitrary[GhostSingleTarget] =
    Arbitrary:
      arbitrary[Target].map(GhostSingleTarget.apply)

  given Cogen[GhostSingleTarget] =
    Cogen[Target].contramap(_.ifu1)

  given Arbitrary[GhostTargetPlusSky] =
    Arbitrary:
      for
        t <- arbitrary[Target]
        c <- arbitrary[Coordinates]
      yield GhostTargetPlusSky(t, c)

  given Cogen[GhostTargetPlusSky] =
    Cogen[(Target, Coordinates)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[GhostSkyPlusTarget] =
    Arbitrary:
      for
        c <- arbitrary[Coordinates]
        t <- arbitrary[Target]
      yield GhostSkyPlusTarget(c, t)

  given Cogen[GhostSkyPlusTarget] =
    Cogen[(Coordinates, Target)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[GhostDualTarget] =
    Arbitrary:
      for
        t0 <- arbitrary[Target]
        t1 <- arbitrary[Target]
      yield GhostDualTarget(t0, t1)

  given Cogen[GhostDualTarget] =
    Cogen[(Target, Target)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[GhostIfuMapping] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[GhostSingleTarget],
        arbitrary[GhostTargetPlusSky],
        arbitrary[GhostSkyPlusTarget],
        arbitrary[GhostDualTarget]
      )

  given Cogen[GhostIfuMapping] =
    Cogen[(
      Int,
      Option[Target],
      Option[(Target, Coordinates)],
      Option[(Coordinates, Target)],
      Option[(Target, Target)]
    )].contramap {
      case GhostSingleTarget(t)     => (0, Some(t), None, None, None)
      case GhostTargetPlusSky(t, c) => (1, None, Some((t, c)), None, None)
      case GhostSkyPlusTarget(c, t) => (2, None, None, Some((c, t)), None)
      case GhostDualTarget(t0, t1)  => (3, None, None, None, Some((t0, t1)))
    }

object ArbGhostIfuMapping extends ArbGhostIfuMapping

