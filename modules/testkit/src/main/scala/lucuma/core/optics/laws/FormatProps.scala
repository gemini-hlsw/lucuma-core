// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws

import cats.Eq
import cats.syntax.all._
import lucuma.core.optics.Format

final case class FormatLaws[A, B](fab: Format[A, B]) {
  def normalize(a: A): IsEq[Option[B]] =
    fab.normalize(a).flatMap(fab.getOption) <-> fab.getOption(a)

  def parseRoundTrip(a: A): IsEq[Option[A]] = {
    val oa = fab.normalize(a)
    oa.flatMap(fab.getOption).map(fab.reverseGet) <-> oa
  }

  def formatRoundTrip(b: B): IsEq[Option[B]] =
    fab.getOption(fab.reverseGet(b)) <-> Some(b)
}

final case class FormatProps[A, B](fab: Format[A, B]) {
  val laws: FormatLaws[A, B] = FormatLaws(fab)

  // True if `a` is parsable but not in normal form. The existence of such a value in our test data
  // will show that `normalize` and `parseRoundTrip` are actually testing something.
  def demonstratesNormalization(a: A)(implicit ev: Eq[A]): Boolean =
    fab.getOption(a).map(fab.reverseGet) match {
      case None     => false
      case Some(aʹ) => a =!= aʹ
    }
}
