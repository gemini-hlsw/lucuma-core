// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics._

import scala.util.Try

package object validation {

  // Convenience type aliases
  type InputFormat[A]          = Format[String, A]
  type ValidNec[E, A]          = Either[NonEmptyChain[E], A]
  type ValidInput[A]           = ValidNec[NonEmptyString, A]
  type ValidFormatNec[E, T, A] = ValidFormat[NonEmptyChain[E], T, A]
  type ValidFormatInput[A]     = ValidFormatNec[NonEmptyString, String, A]

  implicit class StringParseOps(val s: String) extends AnyVal {
    def toBigDecimalOption: Option[BigDecimal] =
      Try(BigDecimal(s)).toOption
  }

  implicit class EitherStringOps[A](val e: Either[String, A]) extends AnyVal {
    def toValidInputUnsafe: ValidInput[A] =
      e.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))
  }

  implicit class EitherNESOps[A](val e: Either[NonEmptyString, A]) extends AnyVal {
    def toValidInput: ValidInput[A] =
      e.leftMap(s => NonEmptyChain(s))
  }

  implicit class ValidFormatInputOps[A](val self: ValidFormatInput[A]) extends AnyVal {

    /**
     * Build `ValidFormatInput` from another one, but allow empty values to become `None`
     */
    def optional: ValidFormatInput[Option[A]] =
      ValidFormatInput(
        (a: String) =>
          if (a.isEmpty)
            none.asRight
          else
            self.getValid(a).map(_.some),
        (a: Option[A]) => a.foldMap(self.reverseGet)
      )

    /**
     * Build `ValidFormatInput[NonEmptyList[A]]` given a `ValidFormatInput[A]`
     */
    def toNel(
      separator: NonEmptyString = ",",
      error:     Option[NonEmptyString] = none // If not set, will show the list of individual errors
    ): ValidFormatInput[NonEmptyList[A]] =
      ValidFormatInput[NonEmptyList[A]](
        _.split(separator).toList.toNel
          .toRight[NonEmptyChain[NonEmptyString]](NonEmptyChain("Cannot be empty"))
          .flatMap(
            _.traverse(self.getValid)
              .leftMap(errorNec => error.fold(errorNec)(e => NonEmptyChain(e)))
          ),
        _.map(self.reverseGet).toList.mkString(separator)
      )
  }
}
