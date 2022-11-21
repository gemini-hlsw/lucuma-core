// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.Order
import cats.data.NonEmptyChain
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString

trait ValidSplitEpiNecInstances {

  /** Build a `ValidSplitEpiNec` that will accept `A < upperBound` */
  def lt[A: Order](
    upperBound: A,
    error:      NonEmptyString
  ): ValidSplitEpiNec[NonEmptyString, A, A] = {
    def get(a: A): EitherNec[NonEmptyString, A] =
      if (a < upperBound) a.asRight
      else NonEmptyChain(error).asLeft
    ValidSplitEpiNec(get, identity)
  }

  /** Build a `ValidSplitEpiNec` that will accept `A <= upperBound` */
  def lte[A: Order](
    upperBound: A,
    error:      NonEmptyString
  ): ValidSplitEpiNec[NonEmptyString, A, A] = {
    def get(a: A): EitherNec[NonEmptyString, A] =
      if (a <= upperBound) a.asRight
      else NonEmptyChain(error).asLeft
    ValidSplitEpiNec(get, identity)
  }

  /** Build a `ValidSplitEpiNec` that will accept `A > upperBound` */
  def gt[A: Order](
    lowerBound: A,
    error:      NonEmptyString
  ): ValidSplitEpiNec[NonEmptyString, A, A] = {
    def get(a: A): EitherNec[NonEmptyString, A] =
      if (a > lowerBound) a.asRight
      else NonEmptyChain(error).asLeft
    ValidSplitEpiNec(get, identity)
  }

  /** Build a `ValidSplitEpiNec` that will accept `A >= upperBound` */
  def gte[A: Order](
    lowerBound: A,
    error:      NonEmptyString
  ): ValidSplitEpiNec[NonEmptyString, A, A] = {
    def get(a: A): EitherNec[NonEmptyString, A] =
      if (a >= lowerBound) a.asRight
      else NonEmptyChain(error).asLeft
    ValidSplitEpiNec(get, identity)
  }
}
