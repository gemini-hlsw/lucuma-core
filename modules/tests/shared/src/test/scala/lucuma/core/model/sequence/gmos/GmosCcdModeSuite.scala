// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class GmosCcdModeSuite extends DisciplineSuite {
  import ArbEnumerated.*
  import lucuma.core.model.sequence.gmos.arb.ArbGmosCcdMode.*

  checkAll("Order[GmosCcdMode]", OrderTests[GmosCcdMode].order)
  checkAll("GmosCcdMode.xBin", LensTests(GmosCcdMode.xBin))
  checkAll("GmosCcdMode.yBin", LensTests(GmosCcdMode.yBin))
  checkAll("GmosCcdMode.ampCount", LensTests(GmosCcdMode.ampCount))
  checkAll("GmosCcdMode.ampGain", LensTests(GmosCcdMode.ampGain))
  checkAll("GmosCcdMode.ampReadMode", LensTests(GmosCcdMode.ampReadMode))
}