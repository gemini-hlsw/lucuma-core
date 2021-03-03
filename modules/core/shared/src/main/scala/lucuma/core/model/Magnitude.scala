// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.math.MagnitudeValue
import monocle.Focus
import monocle.Lens

/**
  * Describes the magnitude of a target on a given band
  */
final case class Magnitude(
  value:  MagnitudeValue,
  band:   MagnitudeBand,
  error:  Option[MagnitudeValue],
  system: MagnitudeSystem
) {
  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Magnitude(${value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${system.tag})"
  }
}

object Magnitude {
  val value: Lens[Magnitude, MagnitudeValue] = Focus[Magnitude](_.value)

  val band: Lens[Magnitude, MagnitudeBand] = Focus[Magnitude](_.band)

  val error: Lens[Magnitude, Option[MagnitudeValue]] = Focus[Magnitude](_.error)

  val system: Lens[Magnitude, MagnitudeSystem] = Focus[Magnitude](_.system)

  /** Secondary constructor. */
  def apply(value: MagnitudeValue, band: MagnitudeBand, error: MagnitudeValue) =
    new Magnitude(value, band, Some(error), band.magnitudeSystem)

  /** Secondary constructor defaulting to no error. */
  def apply(value: MagnitudeValue, band: MagnitudeBand, system: MagnitudeSystem) =
    new Magnitude(value, band, None, system)

  /** Secondary constructor defaulting to no given error. */
  def apply(value: MagnitudeValue, band: MagnitudeBand) =
    new Magnitude(value, band, None, band.magnitudeSystem)

  // by system name, band name, value and error (in that order)
  implicit val MagnitudeOrdering: Order[Magnitude] =
    Order.by(m => (m.system.tag, m.band.tag, m.value, m.error))

  /** group Typeclass Instances */
  implicit val MagnitudeShow: Show[Magnitude] =
    Show.fromToString

}
