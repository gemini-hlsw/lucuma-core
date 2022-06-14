// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline._
import lucuma.core.optics.ValidSplitEpi
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ValidSplitEpiTests[E, A, B] extends Laws {
  val validSplitEpiProps: ValidSplitEpiProps[E, A, B]

  def validSplitEpiLaws(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet(
      "validateLaws",
      "normalize"        -> forAll((a: A) => validSplitEpiProps.normalizeLaw(a)),
      "parse roundtrip"  -> forAll((a: A) => validSplitEpiProps.parseRoundTripLaw(a)),
      "format roundtrip" -> forAll((b: B) => validSplitEpiProps.formatRoundTripLaw(b))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validSplitEpiLawsWith(ga: Gen[A])(implicit
    ee:                         Eq[E],
    ea:                         Eq[A],
    ab:                         Arbitrary[B],
    eb:                         Eq[B]
  ): RuleSet =
    validSplitEpiLaws(ee, Arbitrary(ga), ea, ab, eb)

  def validSplitEpi(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet(
      "validate",
      (validSplitEpiLaws.props :+
        "coverage" -> exists((a: A) =>
          validSplitEpiProps.ensureValidationOrNormalizationCheck(a)
        )): _*
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validSplitEpiWith(ga: Gen[A])(implicit
    ee:                     Eq[E],
    ea:                     Eq[A],
    ab:                     Arbitrary[B],
    eb:                     Eq[B]
  ): RuleSet =
    validSplitEpi(ee, Arbitrary(ga), ea, ab, eb)
}

object ValidSplitEpiTests extends Laws {

  def apply[E, A, B](v: ValidSplitEpi[E, A, B]): ValidSplitEpiTests[E, A, B] =
    new ValidSplitEpiTests[E, A, B] {
      val validSplitEpiProps: ValidSplitEpiProps[E, A, B] = ValidSplitEpiProps(v)
    }

}
