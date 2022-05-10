// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Test

final class FutureExecutionConfigSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbUid._
  import ArbStaticConfig._
  import ArbFutureExecutionConfig._
  import ArbExecutionSequence._

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[FutureExecutionConfig.GmosNorth]", EqTests[FutureExecutionConfig.GmosNorth].eqv)
  checkAll("FutureExecutionConfig.GmosNorth.static",
           LensTests(FutureExecutionConfig.GmosNorth.static)
  )
  checkAll("FutureExecutionConfig.GmosNorth.acquisition",
           LensTests(FutureExecutionConfig.GmosNorth.acquisition)
  )
  checkAll("FutureExecutionConfig.GmosNorth.science",
           LensTests(FutureExecutionConfig.GmosNorth.science)
  )

  checkAll("Eq[FutureExecutionConfig.GmosSouth]", EqTests[FutureExecutionConfig.GmosSouth].eqv)
  checkAll("FutureExecutionConfig.GmosSouth.static",
           LensTests(FutureExecutionConfig.GmosSouth.static)
  )
  checkAll("FutureExecutionConfig.GmosSouth.acquisition",
           LensTests(FutureExecutionConfig.GmosSouth.acquisition)
  )
  checkAll("FutureExecutionConfig.GmosSouth.science",
           LensTests(FutureExecutionConfig.GmosSouth.science)
  )

  checkAll("Eq[FutureExecutionConfig]", EqTests[FutureExecutionConfig].eqv)
  checkAll("FutureExecutionConfig.gmosNorth", PrismTests(FutureExecutionConfig.gmosNorth))
  checkAll("FutureExecutionConfig.gmosSouth", PrismTests(FutureExecutionConfig.gmosSouth))
}
