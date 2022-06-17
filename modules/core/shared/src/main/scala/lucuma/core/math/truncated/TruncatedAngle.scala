// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.truncated

import cats.Eq
import lucuma.core.math.Angle
import lucuma.core.optics.SplitEpi

/**
 * A wrapper for Angle that is rounded to 2 decimal places of precision, in degrees. This is used
 * for input in the UI to allow for lawful ValidFormatInput instances.
 *
 * @param posAangle
 *   The wrapped Angle. Guaranteed to have no more than 2 decimals of precision.
 */
sealed abstract case class TruncatedAngle private (angle: Angle)

object TruncatedAngle {
  def apply(angle: Angle): TruncatedAngle =
    new TruncatedAngle(
      Angle.fromBigDecimalDegrees(
        (angle.toBigDecimalDegrees * 100)
          .setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP) / 100
      )
    ) {}

  val angle: SplitEpi[Angle, TruncatedAngle] =
    SplitEpi[Angle, TruncatedAngle](TruncatedAngle(_), _.angle)

  implicit val truncatedAngleEq: Eq[TruncatedAngle] = Eq.fromUniversalEquals
}
