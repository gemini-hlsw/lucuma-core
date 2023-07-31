// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.util.laws.EnumeratedTests
import monocle.law.discipline.PrismTests
import munit.*

import scala.reflect.ClassTag

enum Theme(val tag: String) derives Enumerated:
   case Light extends Theme("light")
   case Dark extends Theme("dark")

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

  // Derivation
  checkEnumLaws[Theme]

  test("Derived members") {
    val derived = Enumerated[Theme]
    val built = Enumerated.from(Theme.Light, Theme.Dark).withTag(_.tag)

    assertEquals(derived.all, built.all)
    assertEquals(derived.all.map(_.tag), built.all.map(_.tag))
    assertEquals(derived.all.map(_.tag).map(derived.fromTag), built.all.map(_.tag).map(built.fromTag))
  }
}
