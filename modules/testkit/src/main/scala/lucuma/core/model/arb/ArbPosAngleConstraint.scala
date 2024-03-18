// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.model.PosAngleConstraint
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbPosAngleConstraint {

  given Arbitrary[PosAngleConstraint.Fixed] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
      } yield PosAngleConstraint.Fixed(a)
    }

  given Cogen[PosAngleConstraint.Fixed] =
    Cogen[Angle].contramap(_.angle)

  given Arbitrary[PosAngleConstraint.AllowFlip] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
      } yield PosAngleConstraint.AllowFlip(a)
    }

  given Cogen[PosAngleConstraint.AllowFlip] =
    Cogen[Angle].contramap(_.angle)

  given Arbitrary[PosAngleConstraint.ParallacticOverride] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
      } yield PosAngleConstraint.ParallacticOverride(a)
    }

  given Cogen[PosAngleConstraint.ParallacticOverride] =
    Cogen[Angle].contramap(_.angle)

  given Arbitrary[PosAngleConstraint] =
    Arbitrary {
      for {
        f <- arbitrary[PosAngleConstraint.Fixed]
        v <- arbitrary[PosAngleConstraint.AllowFlip]
        p <- Gen.const(PosAngleConstraint.AverageParallactic)
        o <- arbitrary[PosAngleConstraint.ParallacticOverride]
        u <- Gen.const(PosAngleConstraint.Unbounded)
        a <- Gen.oneOf(f, v, p, o, u)
      } yield a
    }

  given Cogen[PosAngleConstraint] =
    Cogen[Option[Option[Either[Angle, Either[Angle, Either[Angle, Angle]]]]]].contramap {
      case PosAngleConstraint.AverageParallactic     => None
      case PosAngleConstraint.Unbounded              => Some(None)
      case PosAngleConstraint.Fixed(a)               => Some(Some(Left(a)))
      case PosAngleConstraint.AllowFlip(a)           => Some(Some(Right(Left(a))))
      case PosAngleConstraint.ParallacticOverride(a) => Some(Some(Right(Right(Left(a)))))
    }
}

object ArbPosAngleConstraint extends ArbPosAngleConstraint
