// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Indication of which entity (if any) is to be charged for program execution time.
 */
enum ChargeClass(val tag: String, val name: String, val description: String) derives Enumerated:
  case NonCharged extends ChargeClass("nonCharged", "Non Charged", "Time that is not charged.")
  case Partner    extends ChargeClass("partner",    "Partner",     "Time charged to a partner country / entity.")
  case Program    extends ChargeClass("program",    "Program",     "Time charged to the science program.")
