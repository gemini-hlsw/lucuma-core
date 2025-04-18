// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for ISS Port Disposition.
 * @group Enumerations (Generated)
 */
enum PortDisposition(
  val tag: String,
  val shortName: String
) derives Enumerated:
  case Side extends PortDisposition("Side", "Side Looking")
  case Bottom extends PortDisposition("Bottom", "Up Looking")
