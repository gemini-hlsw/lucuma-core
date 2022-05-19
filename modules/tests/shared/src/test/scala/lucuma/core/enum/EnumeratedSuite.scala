// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.laws.EnumeratedTests
import monocle.law.discipline.PrismTests
import munit._

import scala.reflect.ClassTag

final class EnumeratedSuite extends DisciplineSuite {

  def checkEnumLaws[A: Enumerated](implicit ct: ClassTag[A]) = {
    val className = ct.runtimeClass.getSimpleName
    checkAll(s"Enumerated[$className]", EnumeratedTests[A].enumerated)
    checkAll(s"Prism[String, $className]", PrismTests(Enumerated.fromTag[A]))
  }

  // Check some Enumerateds
  checkEnumLaws[Site]
  checkEnumLaws[TwilightType]
  checkEnumLaws[EventType]
  checkEnumLaws[KeywordName]
}
