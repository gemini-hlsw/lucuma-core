// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all._
import lucuma.core.optics.ValidFormat
import lucuma.core.optics.laws._

final case class ValidFormatProps[E, T, A](validate: ValidFormat[E, T, A]) {

  def normalizeLaw(t: T): IsEq[Either[E, Either[E, A]]] =
    validate.normalize(t).map(validate.getValid) <-> validate
      .getValid(t)
      .map(Right.apply[E, A])

  def parseRoundTripLaw(t: T): IsEq[Either[E, Either[E, T]]] = {
    val vt = validate.normalize(t)
    vt.map(validate.getValid).map(_.map(validate.reverseGet)) <-> vt.map(Right.apply[E, T])
  }

  def formatRoundTripLaw(a: A): IsEq[Either[E, A]] =
    validate.getValid(validate.reverseGet(a)) <-> Right(a)

  // True if `t` is invalid, or if it is valid but not in normal form. The existence of such a value
  // in our test data will show that `normalize` and `parseRoundTrip` are actually testing something.
  def ensureValidationOrNormalizationCheck(t: T)(implicit ev: Eq[T]): Boolean =
    validate.getValid(t).map(validate.reverseGet) match {
      case Left(_)   => true
      case Right(tʹ) => t =!= tʹ
    }

}
