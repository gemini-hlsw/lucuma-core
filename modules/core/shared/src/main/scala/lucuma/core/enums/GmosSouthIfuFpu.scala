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
 * The GMOS South focal plane units the IFU observing mode offers.
 *
 * See [[GmosNorthIfuFpu]]. The nod & shuffle IFU units the South also carries (`IfuNS2Slits`,
 * `IfuNSBlue`, `IfuNSRed`) are not offered: nothing generates a nod & shuffle sequence yet.
 */
enum GmosSouthIfuFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: GmosSouthFpu,
  val fieldWidth: Angle,
  val skyFieldWidth: Angle
) derives Enumerated, Display:

  case TwoSlits    extends GmosSouthIfuFpu("TwoSlits",    "IFU-2", "IFU 2 Slits",          GmosSouthFpu.Ifu2Slits, Angle.milliarcseconds.reverseGet(7500), Angle.milliarcseconds.reverseGet(3500))
  case OneSlitRed  extends GmosSouthIfuFpu("OneSlitRed",  "IFU-R", "IFU Right Slit (red)", GmosSouthFpu.IfuRed,    Angle.milliarcseconds.reverseGet(3750), Angle.milliarcseconds.reverseGet(1750))
  case OneSlitBlue extends GmosSouthIfuFpu("OneSlitBlue", "IFU-B", "IFU Left Slit (blue)", GmosSouthFpu.IfuBlue,   Angle.milliarcseconds.reverseGet(3750), Angle.milliarcseconds.reverseGet(1750))

object GmosSouthIfuFpu:

  /**
   * The IFU aperture that widens to the given focal plane unit, if any.  The inverse of `fpu`.
   * Empty for every non-IFU unit and for the nod & shuffle IFUs, which this mode does not offer.
   */
  def fromFpu(fpu: GmosSouthFpu): Option[GmosSouthIfuFpu] =
    Enumerated[GmosSouthIfuFpu].all.find(_.fpu === fpu)

