// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.truncated

import cats.Eq
import lucuma.core.math.Angle
import lucuma.core.optics.SplitEpi
import spire.math.Rational

/**
 * A wrapper for Angle that is rounded to 2 decimal places of precision. This is used for input in
 * the UI to allow for lawful ValidFormatInput instances.
 *
 * @param posAangle
 *   The wrapped Angle. Guaranteed to have no more than 2 decimals of precision.
 */
sealed abstract case class TruncatedAngle private (angle: Angle)

object TruncatedAngle {
  def apply(angle: Angle): TruncatedAngle = {
    val microSecs = Rational(angle.toMicroarcseconds, 100).round.toLong * 100
    new TruncatedAngle(Angle.fromMicroarcseconds(microSecs)) {}
  }

  val angle: SplitEpi[Angle, TruncatedAngle] =
    SplitEpi[Angle, TruncatedAngle](TruncatedAngle(_), _.angle)

  implicit val truncatedAngleEq: Eq[TruncatedAngle] = Eq.fromUniversalEquals
}
