// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS Electric Offsetting.
 * @group Enumerations
 */
enum GmosEOffsetting(
  val tag: String,
  val description: String,
  val toBoolean: Boolean
) derives Enumerated:

  case On  extends GmosEOffsetting("On",  "Electronic Offsetting On",  true)
  case Off extends GmosEOffsetting("Off", "Electronic Offsetting Off", false)
