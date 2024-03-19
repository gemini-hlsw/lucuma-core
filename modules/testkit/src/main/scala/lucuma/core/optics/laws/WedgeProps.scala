// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all.*
import lucuma.core.optics.Wedge

final case class WedgeLaws[A, B](fab: Wedge[A, B]) {
  def normalizeA(a: A): IsEq[B] =
    fab.get(fab.normalizeA(a)) <-> fab.get(a)

  def normalizeB(b: B): IsEq[A] =
    fab.reverseGet(fab.normalizeB(b)) <-> fab.reverseGet(b)

  def normalizedReverseGetRoundTrip(b: B): IsEq[B] = {
    val bʹ = fab.normalizeB(b)
    (fab.reverseGet.andThen(fab.get))(bʹ) <-> bʹ
  }

  def normalizedGetRoundTrip(a: A): IsEq[A] = {
    val aʹ = fab.normalizeA(a)
    (fab.reverseGet.compose(fab.get))(aʹ) <-> aʹ
  }
}

final case class WedgeProps[A, B](fab: Wedge[A, B]) {
  val laws: WedgeLaws[A, B] = WedgeLaws(fab)

  // Demonstrate coverage
  def demonstratesCoverageA(a: A)(implicit ev: Eq[A]): Boolean =
    (fab.reverseGet.compose(fab.get))(a) =!= a

  def demonstratesCoverageB(b: B)(implicit ev: Eq[B]): Boolean =
    (fab.reverseGet.andThen(fab.get))(b) =!= b

}
