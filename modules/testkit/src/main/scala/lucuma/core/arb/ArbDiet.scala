// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.arb

import cats.collections.Diet
import cats.collections.Discrete
import cats.collections.Range
import cats.kernel.Order
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import ArbRange.given

trait ArbDiet:

  def genDiet[A: Arbitrary: Discrete: Order]: Gen[Diet[A]] =
    arbitrary[List[Range[A]]].map: rs =>
      val d = rs.foldLeft(Diet.empty[A])(_.addRange(_))
      println(d)
      d

  given [A: Arbitrary: Discrete: Order]: Arbitrary[Diet[A]] =
    Arbitrary(genDiet)

  given [A: Cogen]: Cogen[Diet[A]] =
    Cogen[List[Range[A]]].contramap(_.toIterator.toList)

object ArbDiet extends ArbDiet
