// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.Applicative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck._

import scala.collection.immutable.TreeMap

package object arb {

  implicit class MoreGenOps[A](g: Gen[A]) {

    /** Like `retryUntil` but retries until the specified PartialFunction is defined. */
    def collectUntil[B](f: PartialFunction[A, B]): Gen[B] =
      g.map(a => f.lift(a)).retryUntil(_.isDefined).map(_.get)

    /** Branch randomly. */
    def flatMapOneOf[B](f: A => Gen[B], fs: (A => Gen[B])*): Gen[B] =
      Gen.oneOf(f +: fs).flatMap(g.flatMap)

  }

  // This isn't in scalacheck for whatever reason
  implicit def mapCogen[A: Cogen, B: Cogen]: Cogen[Map[A, B]] =
    Cogen[List[(A, B)]].contramap(_.toList)

  // The above doesn't seem to match a TreeMap unless explicitly cast to a Map
  implicit def treeMapCogen[A: Cogen, B: Cogen]: Cogen[TreeMap[A, B]] =
    Cogen[Map[A, B]].contramap(_.toMap)

  // This doesn't seem to exist anywhere?  https://github.com/non/cats-check
  // would be useful.  All we need is `Applicative` for now though I suppose.
  implicit val applicativeGen = new Applicative[Gen] {
    def ap[A, B](gf: Gen[A => B])(ga: Gen[A]): Gen[B] =
      for {
        f <- gf
        a <- ga
      } yield f(a)

    def pure[A](a: A): Gen[A] =
      Gen.const(a)
  }

  implicit val cogenNonNegativeInt: Cogen[NonNegInt] = Cogen[Int].contramap(_.value)

  implicit val cogenNonEmptyString: Cogen[NonEmptyString] = Cogen[String].contramap(_.value)

}
