// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.arb

import cats.Applicative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.NewType
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.TreeMap

extension[A](g: Gen[A]) {

  /** Like `retryUntil` but retries until the specified PartialFunction is defined. */
  def collectUntil[B](f: PartialFunction[A, B]): Gen[B] =
    g.map(a => f.lift(a)).retryUntil(_.isDefined).map(_.get)

  /** Branch randomly. */
  def flatMapOneOf[B](f: A => Gen[B], fs: (A => Gen[B])*): Gen[B] =
    Gen.oneOf(f +: fs).flatMap(g.flatMap)

}

// This isn't in scalacheck for whatever reason
given [A: Cogen, B: Cogen]: Cogen[Map[A, B]] =
  Cogen[List[(A, B)]].contramap(_.toList)

// The above doesn't seem to match a TreeMap unless explicitly cast to a Map
given [A: Cogen, B: Cogen]: Cogen[TreeMap[A, B]] =
  Cogen[Map[A, B]].contramap(_.toMap)

// This doesn't seem to exist anywhere?  https://github.com/non/cats-check
// would be useful.  All we need is `Applicative` for now though I suppose.
given Applicative[Gen] = new Applicative[Gen] {
  def ap[A, B](gf: Gen[A => B])(ga: Gen[A]): Gen[B] =
    for {
      f <- gf
      a <- ga
    } yield f(a)

  def pure[A](a: A): Gen[A] =
    Gen.const(a)
}

given Cogen[NonNegInt] = Cogen[Int].contramap(_.value)

given Cogen[NonEmptyString] = Cogen[String].contramap(_.value)

@deprecated
def newTypeArbitrary[T: Arbitrary](base: NewType[T]): Arbitrary[base.Type] =
  Arbitrary(arbitrary[T].map(base.apply(_)))

@deprecated
def newTypeCogen[T: Cogen](base: NewType[T]): Cogen[base.Type] =
  Cogen[T].contramap(_.value)
