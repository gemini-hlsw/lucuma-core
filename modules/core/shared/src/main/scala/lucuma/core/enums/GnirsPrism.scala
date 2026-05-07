// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Prism Turret.
 * @group Enumerations (Generated)
 */
enum  GnirsPrism(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated, Display:
    case Mirror extends GnirsPrism("Mirror", "Mirror", "Mirror")
    case Sxd extends GnirsPrism("Sxd", "SXD", "Short cross dispersion")
    case Lxd extends GnirsPrism("Lxd", "LXD", "Long cross dispersion")