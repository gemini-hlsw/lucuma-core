// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.syntax.timespan.*

/**
 * Enumerated type for Flamingos2 reads for engineering.
 * @group Enumerations
 */
enum Igrins2FowlerSamples(val tag: String, val exposureTimeCutoff: TimeSpan)
  derives Enumerated:

  case One     extends Igrins2FowlerSamples("one",     8500.msTimeSpan)
  case Two     extends Igrins2FowlerSamples("two",     11300.msTimeSpan)
  case Four    extends Igrins2FowlerSamples("four",    16000.msTimeSpan)
  case Eight   extends Igrins2FowlerSamples("eight",   24000.msTimeSpan)
  case Sixteen extends Igrins2FowlerSamples("sixteen", 39500.msTimeSpan)
