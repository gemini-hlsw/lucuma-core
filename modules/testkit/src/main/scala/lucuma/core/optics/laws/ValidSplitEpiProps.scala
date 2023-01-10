// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all._
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.optics.laws._

final case class ValidSplitEpiProps[E, A, B](validate: ValidSplitEpi[E, A, B]) {

  def normalizeLaw(a: A): IsEq[Either[E, Either[E, B]]] =
    validate.normalize(a).map(validate.getValid) <-> validate
      .getValid(a)
      .map(Right.apply[E, B])

  def parseRoundTripLaw(a: A): IsEq[Either[E, Either[E, A]]] = {
    val va = validate.normalize(a)
    va.map(validate.getValid).map(_.map(validate.reverseGet)) <-> va.map(Right.apply[E, A])
  }

  def formatRoundTripLaw(b: B): IsEq[Either[E, B]] =
    validate.getValid(validate.reverseGet(b)) <-> Right(b)

  // True if `a` is invalid, or if it is valid but not in normal form. The existence of such a value
  // in our test data will show that `normalize` and `parseRoundTrip` are actually testing something.
  def ensureValidationOrNormalizationCheck(a: A)(implicit ev: Eq[A]): Boolean =
    validate.getValid(a).map(validate.reverseGet) match {
      case Left(_)   => true
      case Right(aʹ) => a =!= aʹ
    }

}
