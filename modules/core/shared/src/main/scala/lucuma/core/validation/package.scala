// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.data.NonEmptyChain
import eu.timepit.refined.api.Refined
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
}
