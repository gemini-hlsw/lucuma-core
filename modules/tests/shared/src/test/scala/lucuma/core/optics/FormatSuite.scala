// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.syntax.all.*
import lucuma.core.optics.laws.discipline.*
import org.scalacheck.Prop.*

final class FormatSuite extends munit.DisciplineSuite {

  // Our example Format injects ints into "positive and even"
  val example: Format[Int, Boolean] =
    Format(n => if (n > 0) Some(n % 2 == 0) else None, b => if (b) 2 else 1)

  // Ensure it's lawful
  checkAll("Formats.HmsDms", FormatTests(example).format)

  test("unsafeGet.consistent with getOption") {
    forAll { (n: Int) =>
      assertEquals(example.getOption(n), Either.catchNonFatal(example.unsafeGet(n)).toOption)
    }
  }

  test("unsafeGet.error message") {
    forAll { (n: Int) =>
      Either.catchNonFatal(example.unsafeGet(n)) match {
        case Left(t)  => assertEquals(t.getMessage, s"unsafeGet failed: $n")
        case Right(_) => assert(true)
      }
    }
  }

}
