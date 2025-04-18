// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.laws
package discipline

import cats.Eq
import cats.laws.discipline.*
import lucuma.core.optics.ValidWedge
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

trait ValidWedgeTests[E, A, B] extends Laws {
  val validatorProps: ValidWedgeProps[E, A, B]

  private def lawsProps(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = List(
    "normalizeValid A"                -> forAll((a: A) => validatorProps.laws.normalizeValidA(a)),
    "normalize B"                     -> forAll((b: B) => validatorProps.laws.normalizeB(b)),
    "reverseGet reverseGet roundtrip" -> forAll((a: A) =>
      validatorProps.laws.normalizedGetValidRoundTrip(a)
    ),
    "normalized get roundtrip"        -> forAll((b: B) =>
      validatorProps.laws.normalizedReverseGetRoundTrip(b)
    )
  )

  private def allProps(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): List[(String, Prop)] = lawsProps ++ List(
    "coverage A" -> exists((a: A) => validatorProps.demonstratesCoverageA(a)),
    "coverage B" -> exists((b: B) => validatorProps.demonstratesCoverageB(b))
  )

  def validWedge(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("ValidWedge", allProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validWedgeWith(ga: Gen[A])(implicit
    ee:                  Eq[E],
    ea:                  Eq[A],
    ab:                  Arbitrary[B],
    eb:                  Eq[B]
  ): RuleSet =
    validWedge(ee, Arbitrary(ga), ea, ab, eb)

  def validWedgeLaws(implicit
    ee: Eq[E],
    aa: Arbitrary[A],
    ea: Eq[A],
    ab: Arbitrary[B],
    eb: Eq[B]
  ): RuleSet =
    new SimpleRuleSet("ValidWedge", lawsProps*)

  /** Convenience constructor that allows passing an explicit generator for input values. */
  def validWedgeLawsWith(ga: Gen[A])(implicit
    ee:                      Eq[E],
    ea:                      Eq[A],
    ab:                      Arbitrary[B],
    eb:                      Eq[B]
  ): RuleSet =
    validWedgeLaws(ee, Arbitrary(ga), ea, ab, eb)
}

object ValidWedgeTests extends Laws {
  def apply[E, A, B](fab: ValidWedge[E, A, B]): ValidWedgeTests[E, A, B] =
    new ValidWedgeTests[E, A, B] {
      val validatorProps = new ValidWedgeProps(fab)
    }
}
