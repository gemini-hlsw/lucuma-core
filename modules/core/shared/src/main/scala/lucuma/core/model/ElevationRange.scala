// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import lucuma.core.optics.SplitEpi
import monocle.Focus
import monocle.Prism
import monocle.macros.GenPrism

enum ElevationRange derives Eq:
  case ByAirMass(min: AirMassBound,  max: AirMassBound) extends ElevationRange
  case ByHourAngle(minHours: HourAngleBound, maxHours: HourAngleBound) extends ElevationRange

object ElevationRange:
  object ByAirMass:
    val DefaultMin: AirMassBound   = AirMassBound.unsafeFromBigDecimal(BigDecimal(1.0))
    val DefaultMax: AirMassBound   = AirMassBound.unsafeFromBigDecimal(BigDecimal(2.0))
    val Default: ElevationRange.ByAirMass = ElevationRange.ByAirMass(DefaultMin, DefaultMax)

    /** @group Typeclass Instances */
    given Eq[ByAirMass] =
      Eq.by(am => (am.min, am.max))

    /** @group Optics */
    val min = Focus[ElevationRange.ByAirMass](_.min)

    /** @group Optics */
    val max = Focus[ElevationRange.ByAirMass](_.max)

    /** @group Optics */
    // Ensures that min <= max by swapping if necessary
    val FromBounds: SplitEpi[(AirMassBound, AirMassBound), ElevationRange.ByAirMass] =
      SplitEpi(
        t => {
          val (min, max) = t
          if (min <= max) ElevationRange.ByAirMass(min, max)
          else ElevationRange.ByAirMass(max, min)
        },
        a => (a.min, a.max)
      )

    /** @group Optics */
    val FromOrderedBounds: Prism[(AirMassBound, AirMassBound), ElevationRange.ByAirMass] =
      Prism[(AirMassBound, AirMassBound), ElevationRange.ByAirMass] { case (min, max) =>
        Option.when(min <= max)(ElevationRange.ByAirMass(min, max))
      }(a => (a.min, a.max))
  end ByAirMass

  object ByHourAngle:
    val DefaultMin: HourAngleBound   = HourAngleBound.Min
    val DefaultMax: HourAngleBound   = HourAngleBound.Max
    val Default: ElevationRange.ByHourAngle = ElevationRange.ByHourAngle(DefaultMin, DefaultMax)

    /** @group Typeclass Instances */
    given Eq[ByHourAngle] =
      Eq.by(hr => (hr.minHours, hr.maxHours))

    /** @group Optics */
    val minHours = Focus[ElevationRange.ByHourAngle](_.minHours)

    /** @group Optics */
    val maxHours = Focus[ElevationRange.ByHourAngle](_.maxHours)

    /** @group Optics */
    // Ensures that minHours <= maxHours by swapping if necessary
    lazy val FromBounds: SplitEpi[(HourAngleBound, HourAngleBound), ByHourAngle] =
      SplitEpi(
        t => {
          val (min, max) = t
          if (min <= max) ByHourAngle(min, max)
          else ByHourAngle(max, min)
        },
        a => (a.minHours, a.maxHours)
      )

    /** @group Optics */
    val FromOrderedBounds: Prism[(HourAngleBound, HourAngleBound), ByHourAngle] =
      Prism[(HourAngleBound, HourAngleBound), ByHourAngle] { case (min, max) =>
        Option.when(min <= max)(ByHourAngle(min, max))
      }(a => (a.minHours, a.maxHours))
  end ByHourAngle

  /** @group Optics */
  val airMass: Prism[ElevationRange, ElevationRange.ByAirMass] =
    GenPrism[ElevationRange, ElevationRange.ByAirMass]

  /** @group Optics */
  val hourAngle: Prism[ElevationRange, ElevationRange.ByHourAngle] =
    GenPrism[ElevationRange, ElevationRange. ByHourAngle]
end ElevationRange