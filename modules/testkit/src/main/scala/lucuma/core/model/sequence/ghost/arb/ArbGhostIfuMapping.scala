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

  given Arbitrary[GhostTarget] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[Target.Sidereal],
        arbitrary[Target.Nonsidereal]
      )

  given Cogen[GhostTarget] =
    Cogen.apply: (seed, t) =>
      t match
        case s: Target.Sidereal    => Cogen[Target.Sidereal].perturb(seed, s)
        case n: Target.Nonsidereal => Cogen[Target.Nonsidereal].perturb(seed.next, n)

  given Arbitrary[SingleTarget] =
    Arbitrary:
      arbitrary[GhostTarget].map(SingleTarget(_))

  given Cogen[SingleTarget] =
    Cogen[GhostTarget].contramap(_.ifu1)

  given Arbitrary[TargetPlusSky] =
    Arbitrary:
      for
        t <- arbitrary[Target.Sidereal]
        c <- arbitrary[Coordinates]
      yield TargetPlusSky(t, c)

  given Cogen[TargetPlusSky] =
    Cogen[(Target.Sidereal, Coordinates)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[SkyPlusTarget] =
    Arbitrary:
      for
        c <- arbitrary[Coordinates]
        t <- arbitrary[Target.Sidereal]
      yield SkyPlusTarget(c, t)

  given Cogen[SkyPlusTarget] =
    Cogen[(Coordinates, Target.Sidereal)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[DualTarget] =
    Arbitrary:
      for
        t0 <- arbitrary[Target.Sidereal]
        t1 <- arbitrary[Target.Sidereal]
      yield DualTarget(t0, t1)

  given Cogen[DualTarget] =
    Cogen[(Target.Sidereal, Target.Sidereal)].contramap: a =>
      (a.ifu1, a.ifu2)

  given Arbitrary[GhostIfuMapping] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[SingleTarget],
        arbitrary[TargetPlusSky],
        arbitrary[SkyPlusTarget],
        arbitrary[DualTarget]
      )

  given Cogen[GhostIfuMapping] =
    Cogen[(
      Int,
      Option[GhostTarget],
      Option[(Target.Sidereal, Coordinates)],
      Option[(Coordinates, Target.Sidereal)],
      Option[(Target.Sidereal, Target.Sidereal)]
    )].contramap {
      case SingleTarget(t)     => (0, Some(t), None, None, None)
      case TargetPlusSky(t, c) => (1, None, Some((t, c)), None, None)
      case SkyPlusTarget(c, t) => (2, None, None, Some((c, t)), None)
      case DualTarget(t0, t1)  => (3, None, None, None, Some((t0, t1)))
    }

object ArbGhostIfuMapping extends ArbGhostIfuMapping