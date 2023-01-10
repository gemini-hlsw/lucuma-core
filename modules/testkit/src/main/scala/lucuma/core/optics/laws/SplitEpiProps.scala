// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all._
import lucuma.core.optics.SplitEpi

final case class SplitEpiLaws[A, B](fab: SplitEpi[A, B]) {
  def normalize(a: A): IsEq[B] =
    fab.get(fab.normalize(a)) <-> fab.get(a)

  def normalizedGetRoundTrip(a: A): IsEq[A] = {
    val aʹ = fab.normalize(a)
    (fab.get.andThen(fab.reverseGet))(aʹ) <-> aʹ
  }

  def reverseGetRoundTrip(b: B): IsEq[B] =
    (fab.reverseGet.andThen(fab.get))(b) <-> b
}

final case class SplitEpiProps[A, B](fab: SplitEpi[A, B]) {
  val laws: SplitEpiLaws[A, B] = SplitEpiLaws(fab)

  // True if `a` is parsable but not in normal form. The existence of such a value in our test data
  // will show that `normalize` and `parseRoundTrup` are actually testing something.
  def demonstratesNormalization(a: A)(implicit ev: Eq[A]): Boolean =
    (fab.get.andThen(fab.reverseGet))(a) =!= a

}
