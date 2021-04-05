// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import monocle.Setter
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._

final case class Person(
  name: String,
  age:  Int
)

object Person {

  val name: Setter[Person, String] =
    Setter[Person, String](f => p => p.copy(name = f(p.name)))

  val age: Setter[Person, Int] =
    Setter[Person, Int](f => p => p.copy(age = f(p.age)))

  implicit val ArbPerson: Arbitrary[Person] =
    Arbitrary {
      for {
        n <- Gen.alphaNumStr
        a <- Gen.posNum[Int]
      } yield Person(n, a)
    }

}

final class SetterOpsSuite extends munit.DisciplineSuite {

  import setter._

  test("SetterOps (1)") {
    forAll { p: Person =>
      // Assign a single property, keeping the other.
      assertEquals((Person.name := "Biff").runS(p).value,  Person("Biff", p.age))
    }
  }

  test("SetterOps Some (1)") {
    forAll { p: Person =>
      // Assign a single property, keeping the other.
      assertEquals((Person.name := Some("Biff")).runS(p).value,  Person("Biff", p.age))
    }
  }

  test("SetterOps None (1)") {
    forAll { p: Person =>
      // Assign a single property, keeping the other.
      assertEquals((Person.name := None).runS(p).value,  p)
    }
  }

  test("SetterOps (2)") {
    forAll { p: Person =>

      // Assign both properties, the initial state is essentially irrelevant.
      val r = (for {
        _ <- Person.name := "Biff"
        _ <- Person.age  := 99
      } yield ()).runS(p).value
      assertEquals(r, Person("Biff", 99))

    }
  }
}
