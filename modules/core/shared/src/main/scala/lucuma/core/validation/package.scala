// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics.*

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

  extension[A](e: Either[String, A])

    /** Convert an `Either[String, A]` to an `Either[Errors, A]` */
    def toEitherErrorsUnsafe: EitherErrors[A] =
      e.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))

  extension[A](e: Either[NonEmptyString, A])

    /** Convert an `Either[NonEmptyString, A]` to an `Either[Errors, A]` */
    def toEitherErrors: EitherErrors[A] =
      e.leftMap(s => NonEmptyChain(s))

  extension[A, B](self: ValidSplitEpi[NonEmptyString, A, B])
    def toErrorsValidSplitEpi: ValidSplitEpi[Errors, A, B] =
      ValidSplitEpi(self.getValid.andThen(_.leftMap(s => NonEmptyChain(s))), self.reverseGet)

  extension[A, B](self: ValidSplitEpi[String, A, B])
    def toErrorsValidSplitEpiUnsafe: ValidSplitEpi[Errors, A, B] =
      ValidSplitEpi(
        self.getValid.andThen(_.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))),
        self.reverseGet
      )

  extension[A, B](self: ValidWedge[NonEmptyString, A, B])
    def toErrorsValidWedge: ValidWedge[Errors, A, B] =
      ValidWedge(self.getValid.andThen(_.leftMap(s => NonEmptyChain(s))), self.reverseGet)

  extension[A, B](self: ValidWedge[String, A, B])
    def toErrorsValidWedgeUnsafe: ValidWedge[Errors, A, B] =
      ValidWedge(
        self.getValid.andThen(_.leftMap(s => NonEmptyChain(NonEmptyString.unsafeFrom(s)))),
        self.reverseGet
      )

  extension[A](self: InputValidWedge[A])
    def withErrorMessage(msg: String => NonEmptyString): InputValidWedge[A] =
      self.withError(str => NonEmptyChain(msg(str)))

    /**
     * Build an `InputValidWedge[NonEmptyList[A]]` given a `InputValidWedge[A]`
     */
    def toNel(separator: NonEmptyString): InputValidWedge[NonEmptyList[A]] =
      InputValidWedge[NonEmptyList[A]](
        _.split(separator).toList.toNel
          .toRight("Must be defined")
          .toEitherErrorsUnsafe
          .flatMap(_.traverse(self.getValid)),
        _.map(self.reverseGet).toList.mkString(separator)
      )

  extension[A](self: InputValidSplitEpi[A])
    def withErrorMessage(msg: String => NonEmptyString): InputValidSplitEpi[A] =
      self.withError(str => NonEmptyChain(msg(str)))

    /**
     * Build an `InputValidSplitEpi[NonEmptyList[A]]` given a `InputValidSplitEpi[A]`
     */
    def toNel(separator: NonEmptyString): InputValidSplitEpi[NonEmptyList[A]] =
      InputValidSplitEpi[NonEmptyList[A]](
        _.split(separator).toList.toNel
          .toRight("Must be defined")
          .toEitherErrorsUnsafe
          .flatMap(_.traverse(self.getValid)),
        _.map(self.reverseGet).toList.mkString(separator)
      )

    def asValidWedge: InputValidWedge[A] = self.asValidWedge
}
