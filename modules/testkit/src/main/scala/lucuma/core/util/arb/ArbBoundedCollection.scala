// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbBoundedCollection {
  val BoundedCollectionLimit = 5

  def genBoundedList[A: Arbitrary](limit: Int): Gen[List[A]] =
    Gen.choose(0, limit).flatMap(sz => Gen.listOfN(sz, arbitrary[A]))

  def genBoundedNonEmptyList[A: Arbitrary](limit: Int): Gen[NonEmptyList[A]] =
    Gen.choose(1, limit).flatMap(sz => Gen.listOfN(sz, arbitrary[A])).map(lst => NonEmptyList.fromListUnsafe(lst))

  def genBoundedVector[A: Arbitrary](limit: Int): Gen[Vector[A]] =
    Gen.choose(0, limit).flatMap(sz => Gen.listOfN(sz, arbitrary[A])).map(_.toVector)

  def genBoundedNonEmptyVector[A: Arbitrary](limit: Int): Gen[NonEmptyVector[A]] =
    Gen.choose(1, limit).flatMap(sz => Gen.listOfN(sz, arbitrary[A])).map(lst => NonEmptyVector.fromVectorUnsafe(lst.toVector))

  given [A: Arbitrary]: Arbitrary[NonEmptyVector[A]] =
    Arbitrary(genBoundedNonEmptyVector[A](BoundedCollectionLimit))

  given [A: Cogen]: Cogen[NonEmptyVector[A]] =
    Cogen[Vector[A]].contramap(_.toVector)

}

object ArbBoundedCollection extends ArbBoundedCollection
