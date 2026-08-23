// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The GMOS North focal plane units the IFU observing mode offers.
 *
 * The instrument also has a left (blue) slit, but it is not offered on its own: a one-slit
 * observation always takes the right (red) slit, as in the OCS template factory (`GmosNIfu`).
 *
 * `fieldWidth` is the target lenslet field across `p`; masking to one pseudo-slit halves it. The
 * field is 5" along `q` either way, so the height lives in the geometry package as
 * `IfuFieldHeight` rather than here.
 */
enum GmosNorthIfuFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: GmosNorthFpu,
  val fieldWidth: Angle
) derives Enumerated, Display:

  case TwoSlits extends GmosNorthIfuFpu("TwoSlits", "IFU-2", "IFU 2 Slits",          GmosNorthFpu.Ifu2Slits, Angle.milliarcseconds.reverseGet(7000))
  case OneSlit  extends GmosNorthIfuFpu("OneSlit",  "IFU-R", "IFU Right Slit (red)", GmosNorthFpu.IfuRed,    Angle.milliarcseconds.reverseGet(3500))
