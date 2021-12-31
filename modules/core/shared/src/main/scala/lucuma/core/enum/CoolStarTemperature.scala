// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import coulomb._
import coulomb.si.Kelvin
import coulomb.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class CoolStarTemperature(
  val name: String,
  val temperature: Quantity[PosBigDecimal, Kelvin]
) extends Product
    with Serializable

object CoolStarTemperature {
    case object T400K extends CoolStarTemperature("400K", BigDecimal(400).withRefinedUnit[Positive, Kelvin])
    case object T600K extends CoolStarTemperature("600K", BigDecimal(600).withRefinedUnit[Positive, Kelvin])
    case object T800K extends CoolStarTemperature("800K", BigDecimal(800).withRefinedUnit[Positive, Kelvin])
    case object T900K extends CoolStarTemperature("900K", BigDecimal(900).withRefinedUnit[Positive, Kelvin])
    case object T1000K extends CoolStarTemperature("1000K", BigDecimal(1000).withRefinedUnit[Positive, Kelvin])
    case object T1200K extends CoolStarTemperature("1200K", BigDecimal(1200).withRefinedUnit[Positive, Kelvin])
    case object T1400K extends CoolStarTemperature("1400K", BigDecimal(1400).withRefinedUnit[Positive, Kelvin])
    case object T1600K extends CoolStarTemperature("1600K",BigDecimal(1600).withRefinedUnit[Positive, Kelvin])
    case object T1800K extends CoolStarTemperature("1800K", BigDecimal(1800).withRefinedUnit[Positive, Kelvin])
    case object T2000K extends CoolStarTemperature("2000K", BigDecimal(2000).withRefinedUnit[Positive, Kelvin])
    case object T2200K extends CoolStarTemperature("2200K", BigDecimal(2200).withRefinedUnit[Positive, Kelvin])
    case object T2400K extends CoolStarTemperature("2400K", BigDecimal(2400).withRefinedUnit[Positive, Kelvin])
    case object T2600K extends CoolStarTemperature("2600K", BigDecimal(2600).withRefinedUnit[Positive, Kelvin])
    case object T2800K extends CoolStarTemperature("2800K", BigDecimal(2800).withRefinedUnit[Positive, Kelvin])

  implicit val enumCoolStarTemperature: Enumerated[CoolStarTemperature] =
    Enumerated.from(T400K, T600K, T800K, T900K, T1000K, T1200K, T1400K, T1600K, T1800K, T2000K, T2200K, T2400K, T2600K, T2800K).withTag(_.name)

  implicit val displayCoolstarTemperature: Display[CoolStarTemperature] =
    Display.byShortName(_.name)
}
