// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class GmosCcdModeSuite extends DisciplineSuite {
  import ArbGmosCcdMode._
  import ArbEnumerated._

  checkAll("Eq[GmosCcdMode]", EqTests[GmosCcdMode].eqv)
  checkAll("GmosCcdMode.xBin", LensTests(GmosCcdMode.xBin))
  checkAll("GmosCcdMode.yBin", LensTests(GmosCcdMode.yBin))
  checkAll("GmosCcdMode.ampCount", LensTests(GmosCcdMode.ampCount))
  checkAll("GmosCcdMode.ampGain", LensTests(GmosCcdMode.ampGain))
  checkAll("GmosCcdMode.ampReadMode", LensTests(GmosCcdMode.ampReadMode))
}
