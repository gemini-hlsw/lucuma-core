// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline._
import lucuma.core.optics.ValidFormat
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

trait ValidFormatTests[E, T, A] extends Laws {
  val validFormatChecks: ValidFormatProps[E, T, A]

  def validFormatLaws(implicit
    at: Arbitrary[T],
    et: Eq[T],
    aa: Arbitrary[A],
    ea: Eq[A],
    ee: Eq[E]
  ): RuleSet =
    new SimpleRuleSet(
      "validateLaws",
      "normalize"        -> forAll((t: T) => validFormatChecks.normalizeLaw(t)),
      "parse roundtrip"  -> forAll((t: T) => validFormatChecks.parseRoundTripLaw(t)),
      "format roundtrip" -> forAll((a: A) => validFormatChecks.formatRoundTripLaw(a))
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validFormatLawsWith(gt: Gen[T])(implicit
    et:                       Eq[T],
    aa:                       Arbitrary[A],
    ea:                       Eq[A],
    ee:                       Eq[E]
  ): RuleSet =
    validFormatLaws(Arbitrary(gt), et, aa, ea, ee)

  def validFormat(implicit
    at: Arbitrary[T],
    et: Eq[T],
    aa: Arbitrary[A],
    ea: Eq[A],
    ee: Eq[E]
  ): RuleSet =
    new SimpleRuleSet(
      "validate",
      (validFormatLaws.props :+
        "coverage" -> exists((t: T) =>
          validFormatChecks.ensureValidationOrNormalizationCheck(t)
        )): _*
    )

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validFormatWith(gt: Gen[T])(implicit
    et:                   Eq[T],
    aa:                   Arbitrary[A],
    ea:                   Eq[A],
    ee:                   Eq[E]
  ): RuleSet =
    validFormat(Arbitrary(gt), et, aa, ea, ee)
}

object ValidFormatTests extends Laws {

  def apply[T, A, E](v: ValidFormat[T, A, E]): ValidFormatTests[T, A, E] =
    new ValidFormatTests[T, A, E] {
      val validFormatChecks: ValidFormatProps[T, A, E] = ValidFormatProps(v)
    }

}
