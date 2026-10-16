// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Order
import lucuma.core.math.Wavelength

/**
 * A GMOS spectroscopy step whose wavelength coverage reaches past the onset of
 * second order light, twice the shortest wavelength reaching the grating.
 * Which steps are affected is decided by the ODB; this just records them.
 *
 * @param onset             where second order light starts
 * @param centralWavelength the step's grating central wavelength
 */
final case class SecondOrderOverlap(
  onset:             Wavelength,
  centralWavelength: Wavelength
)

object SecondOrderOverlap:

  given Order[SecondOrderOverlap] =
    Order.by(a => (a.onset, a.centralWavelength))

  given Ordering[SecondOrderOverlap] =
    Order[SecondOrderOverlap].toOrdering
