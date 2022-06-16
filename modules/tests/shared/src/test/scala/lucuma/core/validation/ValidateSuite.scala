// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all._
import lucuma.core.optics.laws.discipline.ValidFormatTests
import munit.DisciplineSuite
import org.scalacheck.Prop._
import lucuma.core.optics.ValidFormat

final class ValidateSpec extends DisciplineSuite {

  // Our example Validate allows only positive ints and injects into even ints
  val example: ValidFormatNec[String, Int, Boolean] =
    ValidFormat(
      n => if (n > 0) (n % 2 == 0).asRight else NonEmptyChain("Must be > 0").asLeft,
      b => if (b) 2 else 1
    )

  // Ensure it's lawful
  checkAll("Validate.example", ValidFormatTests(example).validFormat)

  test("unsafeGet.consistent with getValid") {
    forAll { (n: Int) =>
      assertEquals(
        example.getValid(n),
        Either
          .catchNonFatal(example.unsafeGet(n))
          .leftMap(_ => NonEmptyChain("Must be > 0"))
      )
    }
  }

  test("unsafeGet.error message") {
    forAll { (n: Int) =>
      Validated.catchNonFatal(example.unsafeGet(n)) match {
        case Invalid(s) => s.getMessage === s"unsafeGet failed: $n"
        case Valid(_)   => true
      }
    }
  }
}
