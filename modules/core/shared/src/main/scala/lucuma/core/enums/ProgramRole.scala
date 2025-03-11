// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for user roles with respect to a given program.
 * @group Enumerations (Generated)
 */
enum ProgramRole(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:
  case PI extends ProgramRole("PI", "PI", "Principal Investigator")
  case GEM extends ProgramRole("GEM", "GEM", "Gemini Contact")
  case NGO extends ProgramRole("NGO", "NGO", "NGO Contact")
