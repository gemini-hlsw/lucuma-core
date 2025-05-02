// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Semigroup
import cats.kernel.laws.discipline.*
import cats.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.arb.ArbCalculatedValue.given
import munit.*

class CalculatedValueSuite extends DisciplineSuite:

  given Semigroup[NonEmptyString] =
    Semigroup.instance[NonEmptyString]((a, b) => NonEmptyString.unsafeFrom(a.value + b.value))

  checkAll("CalculatedValue.CommutativeMonoid", CommutativeMonoidTests[CalculatedValue[Int]].commutativeMonoid)
  checkAll("CalculatedValue.Monoid",            MonoidTests[CalculatedValue[String]].monoid)
  checkAll("CalculatedValue.Semigroup",         SemigroupTests[CalculatedValue[NonEmptyString]].semigroup)
  checkAll("CalculatedValue.Monad",             MonadTests[CalculatedValue].monad[Int, String, Long])
  checkAll("CalculatedValue.Traverse",          TraverseTests[CalculatedValue].traverse[Int, Int, Int, Int, Option, Option])
