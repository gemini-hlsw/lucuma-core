// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.units.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum CoolStarTemperature(
  val tag: String,
  val name: String,
  val temperature: Quantity[PosBigDecimal, Kelvin]
) derives Enumerated:
  case T400K extends CoolStarTemperature("T400_K", "400K", BigDecimal(400).withRefinedUnit[Positive, Kelvin])
  case T600K extends CoolStarTemperature("T600_K", "600K", BigDecimal(600).withRefinedUnit[Positive, Kelvin])
  case T800K extends CoolStarTemperature("T800_K", "800K", BigDecimal(800).withRefinedUnit[Positive, Kelvin])
  case T900K extends CoolStarTemperature("T900_K", "900K", BigDecimal(900).withRefinedUnit[Positive, Kelvin])
  case T1000K extends CoolStarTemperature("T1000_K", "1000K", BigDecimal(1000).withRefinedUnit[Positive, Kelvin])
  case T1200K extends CoolStarTemperature("T1200_K", "1200K", BigDecimal(1200).withRefinedUnit[Positive, Kelvin])
  case T1400K extends CoolStarTemperature("T1400_K", "1400K", BigDecimal(1400).withRefinedUnit[Positive, Kelvin])
  case T1600K extends CoolStarTemperature("T1600_K", "1600K", BigDecimal(1600).withRefinedUnit[Positive, Kelvin])
  case T1800K extends CoolStarTemperature("T1800_K", "1800K", BigDecimal(1800).withRefinedUnit[Positive, Kelvin])
  case T2000K extends CoolStarTemperature("T2000_K", "2000K", BigDecimal(2000).withRefinedUnit[Positive, Kelvin])
  case T2200K extends CoolStarTemperature("T2200_K", "2200K", BigDecimal(2200).withRefinedUnit[Positive, Kelvin])
  case T2400K extends CoolStarTemperature("T2400_K", "2400K", BigDecimal(2400).withRefinedUnit[Positive, Kelvin])
  case T2600K extends CoolStarTemperature("T2600_K", "2600K", BigDecimal(2600).withRefinedUnit[Positive, Kelvin])
  case T2800K extends CoolStarTemperature("T2800_K", "2800K", BigDecimal(2800).withRefinedUnit[Positive, Kelvin])

object CoolStarTemperature:
  given Display[CoolStarTemperature] =
    Display.byShortName(_.name)
