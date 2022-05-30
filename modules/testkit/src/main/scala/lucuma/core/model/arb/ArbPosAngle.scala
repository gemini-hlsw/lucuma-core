// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.PosAngleConstraint
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.Angle
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbPosAngle {

  implicit val fixedPosAngleArb = Arbitrary[PosAngleConstraint.Fixed] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngleConstraint.Fixed(a)
  }

  implicit def fixedPosAngleCogen: Cogen[PosAngleConstraint.Fixed] =
    Cogen[Angle].contramap(_.angle)

  implicit val allowFlipPosAngleArb = Arbitrary[PosAngleConstraint.AllowFlip] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngleConstraint.AllowFlip(a)
  }

  implicit def allowFlipCogen: Cogen[PosAngleConstraint.AllowFlip] =
    Cogen[Angle].contramap(_.angle)

  implicit val parallacticOverridePosAngleArb = Arbitrary[PosAngleConstraint.ParallacticOverride] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngleConstraint.ParallacticOverride(a)
  }

  implicit def parallacticOverridePosAngleCogen: Cogen[PosAngleConstraint.ParallacticOverride] =
    Cogen[Angle].contramap(_.angle)

  implicit val posAngleArb = Arbitrary[PosAngleConstraint] {
    for {
      f <- arbitrary[PosAngleConstraint.Fixed]
      v <- arbitrary[PosAngleConstraint.AllowFlip]
      p <- Gen.const(PosAngleConstraint.AverageParallactic)
      o <- arbitrary[PosAngleConstraint.ParallacticOverride]
      u <- Gen.const(PosAngleConstraint.Unconstrained)
      a <- Gen.oneOf(f, v, p, o, u)
    } yield a
  }

  implicit def posAngleCogen: Cogen[PosAngleConstraint] =
    Cogen[Option[Option[Either[Angle, Either[Angle, Either[Angle, Angle]]]]]].contramap {
      case PosAngleConstraint.AverageParallactic     => None
      case PosAngleConstraint.Unconstrained          => Some(None)
      case PosAngleConstraint.Fixed(a)               => Some(Some(Left(a)))
      case PosAngleConstraint.AllowFlip(a)           => Some(Some(Right(Left(a))))
      case PosAngleConstraint.ParallacticOverride(a) => Some(Some(Right(Right(Left(a)))))
    }
}

object ArbPosAngle extends ArbPosAngle
