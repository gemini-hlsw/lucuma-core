// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import cats.kernel.laws.discipline.OrderTests
import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated._
import monocle.law.discipline.PrismTests
import munit._

import scala.reflect.ClassTag

final class EnumeratedSuite extends DisciplineSuite {

  def checkEnumLaws[A: Enumerated](implicit ct: ClassTag[A]) = {
    val className = ct.runtimeClass.getSimpleName
    checkAll(s"Order[$className]", OrderTests[A].order)
    checkAll(s"Prism[String, $className]", PrismTests(Enumerated.fromTag[A]))
  }

  // Check some Enumerateds
  checkEnumLaws[Site]
  checkEnumLaws[TwilightType]
  checkEnumLaws[EventType]
  checkEnumLaws[KeywordName]
}
