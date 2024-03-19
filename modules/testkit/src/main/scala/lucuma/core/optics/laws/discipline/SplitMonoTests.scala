// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline.*
import lucuma.core.optics.SplitMono
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait SplitMonoTests[A, B] extends Laws {
  val splitMonoProps: SplitMonoProps[A, B]

  private def lawsProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = List(
    "normalize"                -> forAll((b: B) => splitMonoProps.laws.normalize(b)),
    "normalized get roundtrip" -> forAll((b: B) =>
      splitMonoProps.laws.normalizedReverseGetRoundTrip(b)
    ),
    "reverseGet roundtrip"     -> forAll((a: A) => splitMonoProps.laws.getRoundTrip(a))
  )

  private def allProps(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = lawsProps :+ (
    "coverage" -> exists((b: B) => splitMonoProps.demonstratesNormalization(b))
  )

  def splitMono(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("SplitMono", allProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitMonoWith(ga: Gen[A])(implicit
    ea:                 Eq[A],
    ab:                 Arbitrary[B],
    eb:                 Eq[B]
  ): RuleSet =
    splitMono(Arbitrary(ga), ea, ab, eb)

  def splitMonoLaws(implicit
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("SplitMono", lawsProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def splitMonoLawsWith(ga: Gen[A])(implicit
    ea:                     Eq[A],
    ab:                     Arbitrary[B],
    eb:                     Eq[B]
  ): RuleSet =
    splitMonoLaws(Arbitrary(ga), ea, ab, eb)
}

object SplitMonoTests extends Laws {
  def apply[A, B](fab: SplitMono[A, B]): SplitMonoTests[A, B] =
    new SplitMonoTests[A, B] {
      val splitMonoProps = new SplitMonoProps(fab)
    }
}
