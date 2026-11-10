// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import lucuma.core.util.TimeSpan
import munit.FunSuite

class Flamingos2AcquisitionModeSuite extends FunSuite:

  private def seconds(s: BigDecimal): TimeSpan =
    TimeSpan.fromSeconds(s).get

  test("an exposure up to the limit is Bright"):
    assertEquals(Flamingos2AcquisitionMode.defaultFor(seconds(1)), Flamingos2AcquisitionMode.Bright)
    assertEquals(Flamingos2AcquisitionMode.defaultFor(seconds(2)), Flamingos2AcquisitionMode.Bright)

  test("an exposure above the limit is Faint with the default sky offset"):
    assertEquals(Flamingos2AcquisitionMode.defaultFor(seconds(BigDecimal("2.1"))), Flamingos2AcquisitionMode.Faint.Default)
