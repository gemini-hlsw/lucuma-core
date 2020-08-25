// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.math

import coulomb.si._
import coulomb.refined._
import eu.timepit.refined.numeric._
import java.time.ZoneId

package object skycalc {
  val GN =
    Place(
      Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(19.8238068))._1,
      Angle.fromDoubleDegrees(-155.4690550),
      4213.withRefinedUnit[NonNegative, Meter],
      ZoneId.of("Pacific/Honolulu")
    )

  val GS =
    Place(
      Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(-30.2407494))._1,
      Angle.fromDoubleDegrees(-70.7366867),
      2722.withRefinedUnit[NonNegative, Meter],
      ZoneId.of("America/Santiago")
    )
}
