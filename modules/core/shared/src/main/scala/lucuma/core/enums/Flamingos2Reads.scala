// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 reads for engineering.
 * @group Enumerations
 */
enum Flamingos2Reads(val tag: String, val shortName: String, val longName: String, val reads: Int)
  derives Enumerated:

  case Reads_1  extends Flamingos2Reads("reads_1",  "reads_1",  "Reads 1",  1)
  case Reads_3  extends Flamingos2Reads("reads_3",  "reads_3",  "Reads 3",  3)
  case Reads_4  extends Flamingos2Reads("reads_4",  "reads_4",  "Reads 4",  4)
  case Reads_5  extends Flamingos2Reads("reads_5",  "reads_5",  "Reads 5",  5)
  case Reads_6  extends Flamingos2Reads("reads_6",  "reads_6",  "Reads 6",  6)
  case Reads_7  extends Flamingos2Reads("reads_7",  "reads_7",  "Reads 7",  7)
  case Reads_8  extends Flamingos2Reads("reads_8",  "reads_8",  "Reads 8",  8)
  case Reads_9  extends Flamingos2Reads("reads_9",  "reads_9",  "Reads 9",  9)
  case Reads_10 extends Flamingos2Reads("reads_10", "reads_10", "Reads 10", 10)
  case Reads_11 extends Flamingos2Reads("reads_11", "reads_11", "Reads 11", 11)
  case Reads_12 extends Flamingos2Reads("reads_12", "reads_12", "Reads 12", 12)
  case Reads_13 extends Flamingos2Reads("reads_13", "reads_13", "Reads 13", 13)
  case Reads_14 extends Flamingos2Reads("reads_14", "reads_14", "Reads 14", 14)
  case Reads_15 extends Flamingos2Reads("reads_15", "reads_15", "Reads 15", 15)
  case Reads_16 extends Flamingos2Reads("reads_16", "reads_16", "Reads 16", 16)
