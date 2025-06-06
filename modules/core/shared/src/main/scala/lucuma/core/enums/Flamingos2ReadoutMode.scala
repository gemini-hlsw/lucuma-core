// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.util.Enumerated


/**
 * Enumerated type for Flamingos2 readout modes.
 * @group Enumerations (Generated)
 */
enum Flamingos2ReadoutMode(val tag: String, val shortName: String, val longName: String) derives Enumerated:

  /** @group Constructors */
  case Science extends Flamingos2ReadoutMode("Science", "science", "Science")
  /** @group Constructors */
  case Engineering extends Flamingos2ReadoutMode("Engineering", "engineering", "Engineering")
