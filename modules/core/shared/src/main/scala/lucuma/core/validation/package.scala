// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics._

import scala.util.Try

package object validation {
  type DigitCount = Int Refined Interval.Closed[0, 1000]

  // Convenience type aliases
  type Errors = NonEmptyChain[NonEmptyString]

  type EitherNec[E, A] = Either[NonEmptyChain[E], A]
  type EitherErrors[A] = EitherNec[NonEmptyString, A]

  // The `Input` types fix the input type to `String` and the error type to `Errors`.
  type ValidSplitEpiNec[E, A, B] = ValidSplitEpi[NonEmptyChain[E], A, B]
  type InputValidSplitEpi[A]     = ValidSplitEpiNec[NonEmptyString, String, A]

  type ValidWedgeNec[E, A, B] = ValidWedge[NonEmptyChain[E], A, B]
  type InputValidWedge[A]     = ValidWedgeNec[NonEmptyString, String, A]

  type ValidFormatNec[E, A, B] = ValidFormat[NonEmptyChain[E], A, B]
  type InputValidFormat[A]     = ValidFormatNec[NonEmptyString, String, A]

  implicit class StringParseOps(val s: String) extends AnyVal {

    /** Try to parse as a `BigDecimal` */
    def toBigDecimalOption: Option[BigDecimal] =
      Try(BigDecimal(s)).toOption
  }

  implicit class EitherStringOps[A](val e: Either[String, A]) extends AnyVal {

    /** Convert an `Either[String, A]` to an `Either[Errors, A]` */
    def toEitherErrorsUnsafe: EitherErrors[A] =
      e.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))
  }

  implicit class EitherNESOps[A](val e: Either[NonEmptyString, A]) extends AnyVal {

    /** Convert an `Either[NonEmptyString, A]` to an `Either[Errors, A]` */
    def toEitherErrors: EitherErrors[A] =
      e.leftMap(s => NonEmptyChain(s))
  }

  implicit class NESValidSplitEpiOps[A, B](val self: ValidSplitEpi[NonEmptyString, A, B])
      extends AnyVal {
    def toErrorsValidSplitEpi: ValidSplitEpi[Errors, A, B] =
      ValidSplitEpi(self.getValid.andThen(_.leftMap(s => NonEmptyChain(s))), self.reverseGet)
  }

  implicit class StringValidSplitEpiOps[A, B](val self: ValidSplitEpi[String, A, B])
      extends AnyVal {
    def toErrorsValidSplitEpiUnsafe: ValidSplitEpi[Errors, A, B] =
      ValidSplitEpi(
        self.getValid.andThen(_.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))),
        self.reverseGet
      )
  }

  implicit class NESValidWedgeOps[A, B](val self: ValidWedge[NonEmptyString, A, B]) extends AnyVal {
    def toErrorsValidWedge: ValidWedge[Errors, A, B] =
      ValidWedge(self.getValid.andThen(_.leftMap(s => NonEmptyChain(s))), self.reverseGet)
  }

  implicit class StringValidWedgeOps[A, B](val self: ValidWedge[String, A, B]) extends AnyVal {
    def toErrorsValidWedgeUnsafe: ValidWedge[Errors, A, B] =
      ValidWedge(
        self.getValid.andThen(_.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))),
        self.reverseGet
      )
  }

  implicit class InputValidWedgeOps[A](val self: InputValidWedge[A]) extends AnyVal {
    def withErrorMessage(msg: NonEmptyString): InputValidWedge[A] =
      self.withError(NonEmptyChain(msg))

    /**
     * Build an `InputValidWedge[NonEmptyList[A]]` given a `InputValidWedge[A]`
     */
    def toNel(separator: NonEmptyString = ","): InputValidWedge[NonEmptyList[A]] =
      InputValidWedge[NonEmptyList[A]](
        _.split(separator).toList.toNel
          .toRight("Must be defined")
          .toEitherErrorsUnsafe
          .flatMap(_.traverse(self.getValid)),
        _.map(self.reverseGet).toList.mkString(separator)
      )
  }

  implicit class InputValidSplitEpiOps[A](val self: InputValidSplitEpi[A]) extends AnyVal {
    def withErrorMessage(msg: NonEmptyString): InputValidSplitEpi[A] =
      self.withError(NonEmptyChain(msg))

    /**
     * Build an `InputValidSplitEpi[NonEmptyList[A]]` given a `InputValidSplitEpi[A]`
     */
    def toNel(separator: NonEmptyString = ","): InputValidSplitEpi[NonEmptyList[A]] =
      InputValidSplitEpi[NonEmptyList[A]](
        _.split(separator).toList.toNel
          .toRight("Must be defined")
          .toEitherErrorsUnsafe
          .flatMap(_.traverse(self.getValid)),
        _.map(self.reverseGet).toList.mkString(separator)
      )

    def asValidWedge: InputValidWedge[A] = self.asValidWedge
  }
}
