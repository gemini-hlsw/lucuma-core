// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The GMOS North focal plane units the IFU observing mode offers: both pseudo-slits, or either
 * one on its own.
 *
 * `fieldWidth` is the target lenslet field across `p` and `skyFieldWidth` the dedicated sky field
 * ~60" away; masking to one pseudo-slit halves both, whichever slit is kept. The sky field is not
 * half the target field: they are independent apertures (OCS `GmosScienceAreaGeometry.IFUFOVs`).
 * Both are 5" along `q`, so the height lives in the geometry package as `IfuFieldHeight`.
 */
enum GmosNorthIfuFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: GmosNorthFpu,
  val fieldWidth: Angle,
  val skyFieldWidth: Angle
) derives Enumerated, Display:

  case TwoSlits    extends GmosNorthIfuFpu("TwoSlits",    "IFU-2", "IFU 2 Slits",           GmosNorthFpu.Ifu2Slits, Angle.milliarcseconds.reverseGet(7500), Angle.milliarcseconds.reverseGet(3500))
  case OneSlitRed  extends GmosNorthIfuFpu("OneSlitRed",  "IFU-R", "IFU Right Slit (red)",  GmosNorthFpu.IfuRed,    Angle.milliarcseconds.reverseGet(3750), Angle.milliarcseconds.reverseGet(1750))
  case OneSlitBlue extends GmosNorthIfuFpu("OneSlitBlue", "IFU-B", "IFU Left Slit (blue)",  GmosNorthFpu.IfuBlue,   Angle.milliarcseconds.reverseGet(3750), Angle.milliarcseconds.reverseGet(1750))

object GmosNorthIfuFpu:

  /**
   * The IFU aperture that widens to the given focal plane unit, if any.  The inverse of `fpu`.
   * Empty for every non-IFU unit and for the nod & shuffle IFUs, which this mode does not offer.
   */
  def fromFpu(fpu: GmosNorthFpu): Option[GmosNorthIfuFpu] =
    Enumerated[GmosNorthIfuFpu].all.find(_.fpu === fpu)

