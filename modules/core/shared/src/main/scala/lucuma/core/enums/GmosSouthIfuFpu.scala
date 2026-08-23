// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

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
  val fieldWidth: Angle
) derives Enumerated, Display:

  case TwoSlits extends GmosSouthIfuFpu("TwoSlits", "IFU-2", "IFU 2 Slits",          GmosSouthFpu.Ifu2Slits, Angle.milliarcseconds.reverseGet(7000))
  case OneSlit  extends GmosSouthIfuFpu("OneSlit",  "IFU-R", "IFU Right Slit (red)", GmosSouthFpu.IfuRed,    Angle.milliarcseconds.reverseGet(3500))
