// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline._
import lucuma.core.optics.SplitEpi
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait SplitEpiTests[A, B] extends FormatTests[A, B] {
  val splitEpiProps: SplitEpiProps[A, B]

  private def lawsProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = List(
    "normalize"                -> forAll((a: A) => splitEpiProps.laws.normalize(a)),
    "normalized get roundtrip" -> forAll((a: A) => splitEpiProps.laws.normalizedGetRoundTrip(a)),
    "reverseGet roundtrip"     -> forAll((b: B) => splitEpiProps.laws.reverseGetRoundTrip(b))
  )

  private def allProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = lawsProps :+ (
    "coverage" -> exists((a: A) => splitEpiProps.demonstratesNormalization(a))
  )

  def splitEpi(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new DefaultRuleSet("SplitEpi", Some(format), allProps: _*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitEpiWith(ga: Gen[A])(implicit
    ea:                Eq[A],
    ab:                Arbitrary[B],
    eb:                Eq[B]
  ): RuleSet =
    splitEpi(Arbitrary(ga), ea, ab, eb)

  def splitEpiLaws(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new DefaultRuleSet("SplitEpi", Some(formatLaws), lawsProps: _*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitEpiLawsWith(ga: Gen[A])(implicit
    ea:                    Eq[A],
    ab:                    Arbitrary[B],
    eb:                    Eq[B]
  ): RuleSet =
    splitEpiLaws(Arbitrary(ga), ea, ab, eb)
}

object SplitEpiTests extends Laws {
  def apply[A, B](fab: SplitEpi[A, B]): SplitEpiTests[A, B] =
    new SplitEpiTests[A, B] {
      val formatProps   = new FormatProps(fab.asFormat)
      val splitEpiProps = new SplitEpiProps(fab)
    }
}
