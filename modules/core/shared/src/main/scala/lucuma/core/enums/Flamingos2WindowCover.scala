// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 window cover state.
 * @group Enumerations
 */
enum Flamingos2WindowCover(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:

  case Open  extends Flamingos2WindowCover("Open",  "Open",  "Open")
  case Close extends Flamingos2WindowCover("Close", "Close", "Close")
