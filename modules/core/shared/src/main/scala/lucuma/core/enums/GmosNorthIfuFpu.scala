// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The GMOS North focal plane units the IFU observing mode offers.
 *
 * The instrument also has a left (blue) slit, but it is not offered on its own: a one-slit
 * observation always takes the right (red) slit, as in the OCS template factory (`GmosNIfu`).
 */
enum GmosNorthIfuFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: GmosNorthFpu
) derives Enumerated, Display:

  case TwoSlits extends GmosNorthIfuFpu("TwoSlits", "IFU-2", "IFU 2 Slits",          GmosNorthFpu.Ifu2Slits)
  case OneSlit  extends GmosNorthIfuFpu("OneSlit",  "IFU-R", "IFU Right Slit (red)", GmosNorthFpu.IfuRed)
