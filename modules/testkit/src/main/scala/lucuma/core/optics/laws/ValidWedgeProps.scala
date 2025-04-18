// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all.*
import lucuma.core.optics.ValidWedge

final case class ValidWedgeLaws[E, A, B](fab: ValidWedge[E, A, B]) {
  def normalizeValidA(a: A): IsEq[Either[E, B]] =
    fab.normalizeValidA(a).flatMap(fab.getValid) <-> fab.getValid(a)

  def normalizeB(b: B): IsEq[Either[E, A]] =
    fab.normalizeB(b).map(fab.reverseGet) <-> fab.reverseGet(b).asRight

  def normalizedReverseGetRoundTrip(b: B): IsEq[Either[E, B]] = {
    val bʹ = fab.normalizeB(b)
    bʹ.flatMap(fab.reverseGet.andThen(fab.getValid)) <-> bʹ
  }

  def normalizedGetValidRoundTrip(a: A): IsEq[Either[E, A]] = {
    val aʹ = fab.normalizeValidA(a)
    aʹ.flatMap(fab.getValid.andThen(_.map(fab.reverseGet))) <-> aʹ
  }
}

final case class ValidWedgeProps[E, A, B](fab: ValidWedge[E, A, B]) {
  val laws: ValidWedgeLaws[E, A, B] = ValidWedgeLaws(fab)

  // Demonstrate coverage
  def demonstratesCoverageA(a: A)(implicit ev: Eq[A]): Boolean =
    fab.getValid.andThen(_.map(fab.reverseGet))(a).exists(_ =!= a)

  def demonstratesCoverageB(b: B)(implicit evB: Eq[B]): Boolean =
    fab.reverseGet.andThen(fab.getValid)(b).exists(_ =!= b)
}
