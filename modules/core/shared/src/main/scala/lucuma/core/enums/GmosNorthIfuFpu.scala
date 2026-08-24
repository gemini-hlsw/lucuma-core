// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The GMOS North focal plane units the IFU observing mode offers: both pseudo-slits, or either
 * one on its own.
 *
 * `fieldWidth` is the target lenslet field across `p`; masking to one pseudo-slit halves it,
 * whichever slit is kept. The field is 5" along `q` either way, so the height lives in the
 * geometry package as `IfuFieldHeight` rather than here.
 */
enum GmosNorthIfuFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: GmosNorthFpu,
  val fieldWidth: Angle
) derives Enumerated, Display:

  case TwoSlits    extends GmosNorthIfuFpu("TwoSlits",    "IFU-2", "IFU 2 Slits",           GmosNorthFpu.Ifu2Slits, Angle.milliarcseconds.reverseGet(7000))
  case OneSlitRed  extends GmosNorthIfuFpu("OneSlitRed",  "IFU-R", "IFU Right Slit (red)",  GmosNorthFpu.IfuRed,    Angle.milliarcseconds.reverseGet(3500))
  case OneSlitBlue extends GmosNorthIfuFpu("OneSlitBlue", "IFU-B", "IFU Left Slit (blue)",  GmosNorthFpu.IfuBlue,   Angle.milliarcseconds.reverseGet(3500))
