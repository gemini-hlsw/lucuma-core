// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStepConfig {
  import ArbEnumerated.given
  import ArbOffset.given

  given Arbitrary[NonEmptySet[GcalArc]] =
    Arbitrary(
      for {
        a  <- arbitrary[GcalArc]
        as <- arbitrary[List[GcalArc]]
      } yield NonEmptySet.of(a, as*)
    )

  given Cogen[NonEmptySet[GcalArc]] =
    Cogen[(GcalArc, List[GcalArc])].contramap { a =>
      (a.head, a.tail.toList)
    }

  given Arbitrary[StepConfig.Gcal.Lamp] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[GcalContinuum].map(_.asLeft[NonEmptySet[GcalArc]]),
        arbitrary[NonEmptySet[GcalArc]].map(_.asRight[GcalContinuum])
      ).map(StepConfig.Gcal.Lamp.fromEither(_))
    )

  given Cogen[StepConfig.Gcal.Lamp] =
    Cogen[(Either[GcalContinuum, NonEmptySet[GcalArc]])].contramap(_.toEither)

  given Arbitrary[StepConfig.Gcal] = Arbitrary(
    for {
      lamp      <- arbitrary[StepConfig.Gcal.Lamp]
      filter    <- arbitrary[GcalFilter]
      diffuser  <- arbitrary[GcalDiffuser]
      shutter   <- arbitrary[GcalShutter]
    } yield StepConfig.Gcal(lamp, filter, diffuser, shutter)
  )

  given Cogen[StepConfig.Gcal] =
    Cogen[(StepConfig.Gcal.Lamp, GcalFilter, GcalDiffuser, GcalShutter)].contramap(
      c => (c.lamp, c.filter, c.diffuser, c.shutter)
    )

  given Arbitrary[StepConfig.Science] =
    Arbitrary {
      for {
        o <- arbitrary[Offset]
        g <- arbitrary[StepGuideState]
      } yield StepConfig.Science(o, g)
    }

  given Cogen[StepConfig.Science] =
    Cogen[(
      Offset,
      StepGuideState
    )].contramap { a => (
      a.offset,
      a.guiding
    )}

  given Arbitrary[StepConfig.SmartGcal] =
    Arbitrary(arbitrary[SmartGcalType].map(StepConfig.SmartGcal.apply))

  given Cogen[StepConfig.SmartGcal] =
    Cogen[SmartGcalType].contramap(_.smartGcalType)

  given Arbitrary[StepConfig] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(StepConfig.Bias),
        Gen.const(StepConfig.Dark),
        arbitrary[StepConfig.Gcal],
        arbitrary[StepConfig.Science],
        arbitrary[StepConfig.SmartGcal]
      )
    )

  given Cogen[StepConfig] =
    Cogen[Either[Unit, Either[Unit, Either[StepConfig.Gcal, Either[StepConfig.Science, StepConfig.SmartGcal]]]]].contramap {
      case StepConfig.Bias                 => ().asLeft
      case StepConfig.Dark                 => ().asLeft.asRight
      case g @ StepConfig.Gcal(_, _, _, _) => g.asLeft.asRight.asRight
      case s @ StepConfig.Science(_, _)    => s.asLeft.asRight.asRight.asRight
      case m @ StepConfig.SmartGcal(_)     => m.asRight.asRight.asRight.asRight
    }
}

object ArbStepConfig extends ArbStepConfig
