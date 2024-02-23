// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline.*
import lucuma.core.optics.Format
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait FormatTests[A, B] extends Laws {
  val formatProps: FormatProps[A, B]

  private def lawsProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = List(
    "normalize"        -> forAll((a: A) => formatProps.laws.normalize(a)),
    "parse roundtrip"  -> forAll((a: A) => formatProps.laws.parseRoundTrip(a)),
    "format roundtrip" -> forAll((b: B) => formatProps.laws.formatRoundTrip(b))
  )

  private def allProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = lawsProps :+ (
    "coverage" -> exists((a: A) => formatProps.demonstratesNormalization(a))
  )

  def format(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("format", allProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def formatWith(ga: Gen[A])(implicit
    ea:              Eq[A],
    ab:              Arbitrary[B],
    eb:              Eq[B]
  ): RuleSet =
    format(Arbitrary(ga), ea, ab, eb)

  def formatLaws(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("format", lawsProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def formatLawsWith(ga: Gen[A])(implicit
    ea:                  Eq[A],
    ab:                  Arbitrary[B],
    eb:                  Eq[B]
  ): RuleSet =
    formatLaws(Arbitrary(ga), ea, ab, eb)
}

object FormatTests extends Laws {
  def apply[A, B](fab: Format[A, B]): FormatTests[A, B] =
    new FormatTests[A, B] {
      val formatProps: FormatProps[A, B] = FormatProps(fab)
    }
}
